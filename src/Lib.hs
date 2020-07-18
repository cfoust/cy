module Lib
  ( proxyShell
  )
where

import           Control.Monad
import           System.Environment
import           System.Exit
import           System.Posix.Pty
import           System.Process
import qualified Data.ByteString.Char8         as C

-- Continue polling IO and piping stdin/stdout until the program exits.
pipeIO :: (Pty, ProcessHandle) -> IO ()
pipeIO (pty, handle) = do
  threadWaitReadPty pty
  b <- readPty pty
  C.putStrLn b
  code <- getProcessExitCode handle
  when (code == Nothing) $ pipeIO (pty, handle)

proxyShell :: IO ()
proxyShell = do
  env           <- getEnvironment
  (pty, handle) <- spawnWithPty (Just env) True "bash" [] (20, 10)
  writePty pty (C.pack "touch blah && sleep 5 && exit\n")
  pipeIO (pty, handle)
