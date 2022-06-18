{-# LANGUAGE OverloadedStrings #-}

module Seneschal (
    hasValue,
    parallel,
) where

import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Core.Program (
    LongName,
    None,
    Program,
    debugS,
    forkThread,
    queryEnvironmentValue,
    queryOptionValue,
    waitThread,
    writeR,
 )
import Core.Telemetry (encloseSpan)
import Core.Text (Rope, Textual (fromRope, intoRope))
import Data.Maybe (fromMaybe)
import GHC.IO (FilePath)
import Safe (headMay)
import System.Exit (ExitCode (..))
import System.Process.Typed (closed, proc, readProcess, setStdin)
import Prelude (
    Maybe (..),
    Traversable,
    fmap,
    mapM_,
    pure,
    return,
    words,
    ($),
    (<$>),
    (<>),
 )

forkThreadsAndWait :: Traversable f => f a -> (a -> Program t b) -> Program t (f b)
forkThreadsAndWait things action = do
    encloseSpan "Fan out" $ do
        threads <- forM things $ \thing -> forkThread (action thing)
        forM threads waitThread

hasValue :: LongName -> Program t (Maybe Rope)
hasValue v = do
    maybeStr <- queryOptionValue v
    pure $ intoRope <$> maybeStr

{-
Thin wrapper around **typed-process**'s `readProcess` so that the command
to be executed can be logged. Bit of an annoyance that the command and the
arguments have to be specified to `proc` separately, but that's _execvp(3)_
for you.
TODO this could potentially move to the **unbeliever** library
-}
-- Shamelessly stolen from https://github.com/aesiniath/publish/blob/main/src/Utilities.hs#L41-L61
execProcess :: Rope -> Program t (ExitCode, Rope, Rope)
execProcess cmd = do
    shEnv <- queryEnvironmentValue "SHELL"
    shOpt <- hasValue "shell"
    let cmdStr = fromRope cmd
        cmdBinName = intoRope $ fromMaybe "" $ headMay $ words $ fromRope cmd
        shell :: FilePath
        shell = fromRope $ case (shOpt, shEnv) of
            (Nothing, Nothing) -> "bash"
            (Nothing, Just shEnvName) -> shEnvName
            (Just shName, _) -> fromRope shName
        -- It's conceivable that some shell could use an argument other than -c
        -- for this, but neither bash, zsh, nor fish do. so. ¯\_(ツ)_/¯
        task = proc shell ("-c" : [cmdStr])
        task' = setStdin closed task
     in do
            encloseSpan ("Exec Process " <> cmdBinName) $ do
                debugS "command" task'
                (exit, out, err) <- liftIO $ readProcess task'
                return (exit, intoRope out, intoRope err)

parallel :: [Rope] -> Program None ()
parallel cmds = do
    encloseSpan "parallel" $ do
        outputs <- forkThreadsAndWait cmds execProcess
        let outputsOuts = fmap (\(_exitCode, out, err) -> out <> "\n" <> err) outputs
         in mapM_ writeR outputsOuts
