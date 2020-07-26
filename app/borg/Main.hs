module Main where

import           System.IO
import qualified Data.ByteString.Lazy          as L
import           Types
import           Data.Binary

parseFile :: Binary a => L.ByteString -> [a] -> [a]
parseFile s results = case result of
  Left  _                     -> results
  Right (remainder, _, value) -> parseFile remainder (results ++ [value])
  where result = decodeOrFail s

main :: IO ()
main = do
  outFile <- openBinaryFile "test.borg" ReadMode
  str     <- L.hGetContents outFile
  print $ (parseFile str [] :: [TimeMutation])
