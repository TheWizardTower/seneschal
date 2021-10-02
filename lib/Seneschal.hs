{-# LANGUAGE OverloadedStrings #-}

module Seneschal (
  parallel,
) where

import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Core.Program (None, Program, forkThread, waitThread, writeR)
import Core.Text (Rope, Textual (fromRope, intoRope), breakLines)
import System.Process (readProcess)
import Prelude (Traversable, fmap, mapM_, ($), (<$>), (<>))

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
  let cmdLines = breakLines cmd
      cmdStrs = fmap fromRope cmdLines
   in liftIO $ intoRope <$> readProcess "bash" (["-c"] <> cmdStrs) ""

