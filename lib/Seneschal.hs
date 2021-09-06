module Seneschal (
runShellCommandInParallel
) where

import Control.Concurrent.Async ( mapConcurrently )
import System.Process ( readProcess )

runShellCommandInParallel :: [String] -> IO ()
runShellCommandInParallel cmds = do
  outputs <- mapConcurrently (\cmd -> do 
    let (binName, args) = tokenizeString cmd in readProcess binName args "") cmds
  mapM_ putStrLn outputs
  

-- This function brings Dishonor upon my House. Refactor it to use the Either
-- monad, so my ancestors may look down upon me from Sto'Vo'Kor with pride.

tokenizeString :: String -> (String, [String])
tokenizeString cmd = 
  let cmdList = words cmd
      binName = head cmdList
      args = tail cmdList in
        (binName, args)

