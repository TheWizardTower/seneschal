{-# LANGUAGE OverloadedStrings #-}

module Seneschal (
  hasValue,
  parallel,
) where

import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Core.Program (None, Program, debugS, forkThread, waitThread, writeR)
import Core.Text (Rope, Textual (fromRope, intoRope))
import System.Exit (ExitCode (..))
import System.Process.Typed (closed, proc, readProcess, setStdin)
import Prelude (Traversable, fmap, mapM_, return, ($), (<>))

forkThreadsAndWait :: Traversable f => f a -> (a -> Program t b) -> Program t (f b)
forkThreadsAndWait things action = do
  threads <- forM things $ \thing -> forkThread (action thing)
  forM threads waitThread

hasValue :: LongName -> Parameters -> Maybe Rope
hasValue v params = intoRope <$> lookupOptionValue v params

{-
Thin wrapper around **typed-process**'s `readProcess` so that the command
to be executed can be logged. Bit of an annoyance that the command and the
arguments have to be specified to `proc` separately, but that's _execvp(3)_
for you.
TODO this could potentially move to the **unbeliever** library
-}
-- Shamelessly stolen from https://github.com/aesiniath/publish/blob/main/src/Utilities.hs#L41-L61
execProcess :: Rope -> Program t (ExitCode, Rope, Rope)
execProcess cmd =
  let cmdStr = fromRope cmd
      task = proc "bash" ("-c" : [cmdStr])
      task' = setStdin closed task
   in do
        debugS "command" task'

        (exit, out, err) <- liftIO $ do
          readProcess task'

        return (exit, intoRope out, intoRope err)

parallel :: [Rope] -> Program None ()
parallel cmds = do
  outputs <- forkThreadsAndWait cmds execProcess
  let outputsOuts = fmap (\(_exitCode, out, err) -> out <> "\n" <> err) outputs
   in mapM_ writeR outputsOuts