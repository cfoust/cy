module Lib
  ( proxyShell
  )
where

import           System.Posix.Pty
import           System.Process
import           System.Exit
import qualified Data.ByteString.Char8         as C

-- Continue polling IO and piping stdin/stdout until the program exits.
pipeIO :: (Pty, ProcessHandle) -> IO ()
pipeIO (pty, handle) = do
  threadWaitReadPty pty
  b <- readPty pty
  C.putStrLn b
  code <- getProcessExitCode handle
  case code of
    Nothing -> pipeIO (pty, handle)
    Just n  -> return ()

proxyShell :: IO ()
proxyShell = do
  (pty, handle) <- spawnWithPty
    (Just [("SHELL", "/bin/bash"), ("HOME", "/home/caleb")])
    True
    "bash"
    []
    (20, 10)
  writePty pty (C.pack "touch blah\n")
  pipeIO (pty, handle)
