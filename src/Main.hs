module Main where

import Seneschal ( runShellCommandInParallel )
import Prelude (IO, putStrLn)

main :: IO ()
main = do
    putStrLn "Beginning"
    putStrLn ""
    runShellCommandInParallel ["echo 'Hello!'", "seq 1 10", "pwd"]


