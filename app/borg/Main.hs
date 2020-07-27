{-# LANGUAGE OverloadedStrings #-}
module Main where

import           System.IO
import qualified Data.ByteString.Lazy          as L
import           Data.Binary
import qualified Data.ByteString.Char8         as C
import qualified Data.Text.Encoding            as E
import qualified Data.Text                     as T

import           Emulator.Types
import           Emulator.Parser
import           Types

parseMutation :: TimeMutation -> [TerminalMutation]
parseMutation (TimeMutation _ x) = case x of
  Output z ->
    (case result of
      Left  _       -> []
      Right results -> results
    )
    where result = parseOutput (E.decodeUtf8 z)
  Input _        -> []
  WindowSize _ _ -> []

parseFile :: Binary a => L.ByteString -> [a] -> [a]
parseFile s results = case result of
  Left  _                     -> results
  Right (remainder, _, value) -> parseFile remainder (results ++ [value])
  where result = decodeOrFail s

main :: IO ()
main = do
  outFile   <- openBinaryFile "test.borg" ReadMode
  str       <- L.hGetContents outFile
  mutations <- pure $ (parseFile str [] :: [TimeMutation]) >>= parseMutation
  print mutations
  return ()
