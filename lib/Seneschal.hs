{-# LANGUAGE OverloadedStrings #-}

module Seneschal (
  parallel,
) where

import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Core.Program (None, Program, writeR, forkThread, waitThread)
import Core.Text (Rope, Textual (fromRope, intoRope), breakWords)
import Safe (headMay)
import System.Process (readProcess)
import Prelude (Traversable, Maybe (..), fmap, mapM_, return, tail, ($), (<$>))

forkThreadsAndWait :: Traversable f => (f a) -> (a -> Program t b) -> Program t (f b)
forkThreadsAndWait things action = do
  threads <- forM things $ \thing -> forkThread (action thing)
  forM threads waitThread

parallel :: [Rope] -> Program None ()
parallel cmds = do
  outputs <- forkThreadsAndWait cmds parallelCommand
  mapM_ writeR outputs

parallelCommand :: Rope -> Program None Rope
parallelCommand cmd =
  let tokenizeResult = tokenizeString cmd
      result = case tokenizeResult of
        Just (binName, args) ->
          let binNameS = fromRope binName
              argsS = fmap fromRope args
           in liftIO $ intoRope <$> readProcess binNameS argsS ""
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
