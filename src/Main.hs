{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Core.Program (
    Config,
    None (..),
    Options (..),
    ParameterValue (..),
    Program,
    Version (..),
    configure,
    critical,
    executeWith,
    fromPackage,
    inputEntire,
    queryOptionValue,
    simpleConfig,
    terminate,
 )
import Core.System (stdin)
import Core.Telemetry
import Core.Text (
    Rope,
    breakLines,
    emptyRope,
    fromRope,
    intoRope,
    quote,
 )
import Data.Aeson (decode)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T (replace)
import Seneschal (hasValue, parallel)
import TraceSpanData
import Prelude (IO, Maybe (..), ($), (<>), (==))

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
            (Value "{}")
            [quote|
       String to use as the find-and-replace target in the stdin input or prefix value.
       |]
        , Variable
            "SHELL"
            [quote|
            Seneschal uses the $SHELL environment variable to detect what shell
            to run your commands in. This can be overridden with --shell
            |]
        , Option
            "shell"
            (Just 'S')
            Empty
            [quote|
           Shell to execute your command (or commands) in. By default, seneschal
           executes in the shell you're running, or the shell that's calling
           Seneschal.
           |]
        , Variable "HONEYCOMB_TOKEN" [quote|Honeycomb Token to use when sending telemetry up.|]
        , Variable "TRACE_SPAN_DATA" [quote|JSON Blob for the trace span data value|]
        ]

main :: IO ()
main = do
    context <-
        configure
            version
            None
            myConfig
    context' <- initializeTelemetry [consoleExporter, structuredExporter, honeycombExporter] context
    executeWith context' program

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

startTelem :: Program None ()
startTelem = do
    tsdOption <- queryOptionValue "TRACE_SPAN_DATA"
    case tsdOption of
        Nothing -> do
            beginTrace $
                encloseSpan "seneschal" program
        Just tsdJson ->
            do
                let decodedTSD :: Maybe TraceSpanData
                    decodedTSD = decode $ fromRope tsdJson
                    tsdConcrete = fromMaybe emptyTSD decodedTSD
                    tidMaybe = traceID tsdConcrete
                 in do
                        case tidMaybe of
                            Nothing -> do
                                critical "No Trace ID found."
                                terminate 127
                            Just tid -> usingTrace' tid program

program :: Program None ()
program = do
    stdinBytes <- inputEntire stdin
    prefixOpt <- hasValue "prefix"
    replaceStrOpt <- hasValue "replace-str"
    let stdinLines = breakLines $ intoRope stdinBytes
     in parallel $
            stdinLines <&> \line ->
                case (prefixOpt, replaceStrOpt) of
                    -- It'd be nice if the "{}" value in this case statement
                    -- came from something more intelligent, like the actual
                    -- default value.
                    (Just prefix, Nothing) -> let replLine = replace "{}" line prefix in if replLine == line then prefix <> " " <> line else replLine
                    (Just prefix, Just needle) -> replace needle line prefix
                    (_, _) -> line
