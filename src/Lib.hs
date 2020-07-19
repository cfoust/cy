module Lib
  ( proxyShell
  )
where

import           Control.Concurrent             ( forkIO )
import           Control.Monad
import           System.Environment
import           System.Exit
import           System.IO
import           System.Posix.Pty
import           System.Process
import qualified Data.ByteString.Char8         as C
import           Data.ByteString.Internal       ( toForeignPtr )
import           Foreign.ForeignPtr             ( withForeignPtr )
import           Foreign.Marshal.Alloc          ( allocaBytes )


isDone :: ProcessHandle -> IO Bool
isDone handle = do
  code <- getProcessExitCode handle
  return (code /= Nothing)

-- stdin -> pty
pipeStdin :: Pty -> ProcessHandle -> IO ()
pipeStdin pty handle = do
  done <- isDone handle
  if done
    then return ()
    else do
      haveInput <- hWaitForInput stdin 200
      when haveInput $ do
        char <- hGetChar stdin
        writePty pty (C.singleton char)
      pipeStdin pty handle

-- pty -> stdout
pipeStdout :: Pty -> ProcessHandle -> IO ()
pipeStdout pty handle = do
  done <- isDone handle
  if done
    then return ()
    else do
      threadWaitReadPty pty
      chars <- readPty pty
      C.hPut stdout chars
      pipeStdout pty handle

proxyShell :: IO ()
proxyShell = do
  env <- getEnvironment

  -- Make it so we can proxy characters from stdin right away
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  hSetBinaryMode stdout True

  (pty, handle) <- spawnWithPty (Just env) True "bash" [] (20, 10)

  forkIO $ pipeStdin pty handle
  forkIO $ pipeStdout pty handle
  waitForProcess handle
  return ()
