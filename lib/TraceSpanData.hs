{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
-- I'm evil, but I've made my peace with it.
{-# OPTIONS_GHC -Wno-orphans #-}

module TraceSpanData where

import Core.Telemetry
import Core.Text

import Chrono.TimeStamp
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import GHC.Int

-- Stolen from war-correspondant. Chuck it up to stackage and depend on it.

deriving instance Generic Span
deriving instance Generic Trace
instance ToJSON Span where
    toJSON (Span spanIdentifier) = String (fromRope spanIdentifier :: Text)
instance ToJSON Trace where
    toJSON (Trace traceIdentifier) = String (fromRope traceIdentifier :: Text)

instance FromJSON Trace where
    parseJSON = withText "Trace" (pure . intoTrace . intoRope)
      where
        intoTrace :: Rope -> Trace
        intoTrace tid = Trace tid

instance FromJSON Span where
    parseJSON = withText "Trace" (pure . intoSpan . intoRope)
      where
        intoSpan :: Rope -> Span
        intoSpan sid = Span sid

data TraceSpanData = TraceSpanData
    { start :: TimeStamp
    , traceID :: Maybe Trace
    , spanID :: Maybe Span
    , spanLabel :: Rope
    , runtime :: Maybe Int64
    , parentSpanID :: Maybe Span
    }
    deriving (Eq, Show, Generic)

emptyTSD :: TraceSpanData
emptyTSD =
    TraceSpanData
        { start = TimeStamp 0
        , traceID = Nothing
        , spanID = Nothing
        , spanLabel = ""
        , runtime = Nothing
        , parentSpanID = Nothing
        }

instance ToJSON TraceSpanData where
    toJSON tsd =
        object
            [ "start" .= unTimeStamp (start tsd)
            , --    [ "start" .= (start tsd)
              "traceID" .= traceID tsd
            , "spanID" .= spanID tsd
            , "spanLabel" .= (fromRope $ spanLabel tsd :: Text)
            , "runtime" .= runtime tsd
            , "parentSpanID" .= parentSpanID tsd
            ]

instance FromJSON TraceSpanData where
    parseJSON (Object v) =
        TraceSpanData
            <$> (TimeStamp <$> v .: "start")
            <*> v .: "traceID"
            <*> v .: "spanID"
            <*> v .: "spanLabel"
            <*> v .: "runtime"
            <*> v .: "parentSpanID"
    -- Just to silence a very annoying build warning
    parseJSON _ = error "Invalid object"
