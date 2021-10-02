{-# LANGUAGE OverloadedStrings #-}

module Seneschal (
  runShellCommandInParallel,
) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad.IO.Class (liftIO)
import Core.Program (writeR)
import Core.Program.Execute (None, Program)
import Core.Text.Rope (Rope, Textual (fromRope, intoRope))
import Core.Text.Utilities (breakWords)
import Safe (headMay)
import System.Process (readProcess)

runShellCommandInParallel :: [Rope] -> Program None ()
runShellCommandInParallel cmds = do
  -- It'd be nice if I had a `core-program` native implementation of
  -- mapConcurrently. There probably is one, I just don't know about it.
  outputs <- liftIO $ mapConcurrently parallelCommand cmds
  mapM_ writeR outputs

parallelCommand :: Rope -> IO Rope
parallelCommand cmd =
  let tokenizeResult = tokenizeString cmd
      result = case tokenizeResult of
        Just (binName, args) ->
          let binNameS = fromRope binName
              argsS = fmap fromRope args
           in intoRope <$> readProcess binNameS argsS ""
        Nothing -> return ""
   in result

tokenizeString :: Rope -> Maybe (Rope, [Rope])
tokenizeString cmd =
  let cmdList = breakWords cmd
      binNameM = headMay cmdList
      args = tail cmdList
      result = case binNameM of
        Just a -> Just (a, args)
        Nothing -> Nothing
   in result
