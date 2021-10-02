{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Core.Data (lookupKeyValue)
import Core.Program (
    None (..),
    Options (..),
    ParameterValue (..),
    Program,
    Version (..),
    configure,
    executeWith,
    fromPackage,
    getCommandLine,
    inputEntire,
    parameterValuesFrom,
    simpleConfig,
    write,
 )
import Core.System (stdin)
import Core.Text (Rope, breakLines, emptyRope, intoRope, quote)
import Seneschal (parallel)
import Prelude (IO, Maybe (..), fmap, ($), (<>))

version :: Version
version = $(fromPackage)

main :: IO ()
main = do
    context <-
        configure
            version
            None
            ( simpleConfig
                [ Option
                    "prefix"
                    (Just 'p')
                    (Value "")
                    [quote|
        Specify a command prefix to, well, prefix to the input fed in via stdin.
        |]
                , Option
                    "replace-str"
                    (Just 'I')
                    (Value "'{}'")
                    [quote|
       String to use as the find-and-replace target in the stdin input or prefix value.
       |]
                ]
            )
    executeWith context program

parameterToRope :: ParameterValue -> Rope
parameterToRope param = case param of
    Value p -> intoRope p
    Empty -> emptyRope

program :: Program None ()
program = do
    params <- getCommandLine
    let result = lookupKeyValue "prefix" (parameterValuesFrom params)
    case result of
        Nothing -> do
            write "Beginning Super Awesome Test Suite\n"
            parallel ["echo 'Hello!'", "seq 1 10", "pwd", "ping -c 5 gog.com"]
        Just prefix -> do
            stdinBytes <- inputEntire stdin
            let prefixRope = parameterToRope prefix
                stdinPrefixed = fmap (\a -> prefixRope <> " " <> a) $ breakLines $ intoRope stdinBytes
             in parallel stdinPrefixed
