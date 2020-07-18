module Lib
    ( someFunc
    ) where

import System.Posix.Pty
import System.Process
import System.Exit
import qualified Data.ByteString.Char8 as C

someFunc :: IO ()
someFunc = do
  (pty, handle) <- spawnWithPty (Just [("SHELL", "bash")]) True "bash" ["--help"] (20, 10)
  threadWaitReadPty pty
  output <- readPty pty
  exit <- waitForProcess handle
  case exit of
    ExitSuccess -> putStrLn "ok"
    ExitFailure n -> putStrLn "failed"
  C.putStrLn output
