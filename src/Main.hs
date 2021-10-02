{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Core.Data (lookupKeyValue)
import Core.Program (
    Config,
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
 )
import Core.System (stdin)
import Core.Text (Rope, breakLines, emptyRope, fromRope, intoRope, quote)
import qualified Data.Text as T (replace)
import Seneschal (parallel)
import Prelude (IO, Maybe (..), fmap, ($), (<>))

version :: Version
version = $(fromPackage)

myConfig :: Config
myConfig =
    simpleConfig
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
            -- Sadly, when you run `lookupKeyValue "replace-str", this
            -- default value gets completely ignored. :(
            -- TODO: write a function that returns the default value
            --       when the caller does to specify a --replace-str/-I
            --       value
            (Value "{}")
            [quote|
       String to use as the find-and-replace target in the stdin input or prefix value.
       |]
        ]

main :: IO ()
main = do
    context <-
        configure
            version
            None
            myConfig
    executeWith context program

parameterToRope :: ParameterValue -> Rope
parameterToRope param = case param of
    Value p -> intoRope p
    Empty -> emptyRope

-- This is really ugly, and I can't even imagine the runtime cost I'm taking on
-- this, but I was in a hurry and wanted to get something running.
replace :: Rope -> Rope -> Rope -> Rope
replace needle replacement haystack = do
    let needleBS = fromRope needle
        replacementBS = fromRope replacement
        haystackBS = fromRope haystack
     in intoRope $ T.replace needleBS replacementBS haystackBS

program :: Program None ()
program = do
    params <- getCommandLine
    stdinBytes <- inputEntire stdin
    let prefixCheck = lookupKeyValue "prefix" (parameterValuesFrom params)
        substCheck = lookupKeyValue "replace-str" (parameterValuesFrom params)
        stdinLines = breakLines $ intoRope stdinBytes
    case prefixCheck of
        Nothing -> do
            parallel stdinLines
        Just prefix -> do
            let prefixRope = parameterToRope prefix
            case substCheck of
                Nothing -> do
                    let stdinPrefixed = fmap (\a -> prefixRope <> " " <> a) stdinLines
                     in parallel stdinPrefixed
                Just needleParam -> do
                    let needleRope = parameterToRope needleParam
                        prefixSubst = fmap (\replacement -> replace needleRope replacement prefixRope) stdinLines
                     in parallel prefixSubst
