{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Lib
    ( run
    ) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Parser

run :: String -> IO ()
run f = do
  hw <- parseHomework f <$> TIO.readFile f
  case hw of
    Left msg -> error msg
    Right hw -> putStrLn $ show hw
