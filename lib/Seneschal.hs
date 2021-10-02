{-# LANGUAGE OverloadedStrings #-}

module Seneschal (
  runShellCommandInParallel,
) where

import Control.Concurrent.Async (mapConcurrently)
import Core.Text.Rope (Rope, Textual (fromRope, intoRope))
import Core.Text.Utilities (breakWords)
import Safe (headMay)
import System.Process (readProcess)

runShellCommandInParallel :: [Rope] -> IO ()
runShellCommandInParallel cmds = do
  outputs <- mapConcurrently parallelCommand cmds
  mapM_ print outputs

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
