{-# LANGUAGE OverloadedStrings #-}

module Main where

import Core.Program (None, Program, execute, write)
import Seneschal (runShellCommandInParallel)
import Prelude (IO)

main :: IO ()
main = execute program

program :: Program None ()
program = do
    write "Beginning"
    write ""
    runShellCommandInParallel ["echo 'Hello!'", "seq 1 10", "pwd", "ping -c 5 gog.com"]
