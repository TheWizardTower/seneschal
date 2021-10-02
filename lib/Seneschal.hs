module Seneschal (
  runShellCommandInParallel,
) where

import Control.Concurrent.Async (mapConcurrently)
import Safe (headMay)
import System.Process (readProcess)

runShellCommandInParallel :: [String] -> IO ()
runShellCommandInParallel cmds = do
  outputs <- mapConcurrently parallelCommand cmds
  mapM_ putStrLn outputs

parallelCommand :: String -> IO String
parallelCommand cmd =
  let tokenizeResult = tokenizeString cmd
      result = case tokenizeResult of
        Just (binName, args) -> readProcess binName args ""
        Nothing -> return ""
   in result

tokenizeString :: String -> Maybe (String, [String])
tokenizeString cmd =
  let cmdList = words cmd
      binNameM = headMay cmdList
      args = tail cmdList
      result = case binNameM of
        Just a -> Just (a, args)
        Nothing -> Nothing
   in result
