module Lib
  ( proxyShell
  )
where

import           Control.Concurrent             ( forkIO )
import           Control.Monad
import           System.Environment
import           System.Exit
import           System.Signal                  ( installHandler )
import           System.Posix.Signals.Exts      ( sigWINCH )
import           System.IO
import           System.Posix.Pty
import           System.Process
import qualified Data.ByteString.Char8         as C
import           System.Console.Terminal.Size   ( size
                                                , height
                                                , width
                                                )


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

handleSize pty signal = do
  winSize <- size
  case winSize of
    Just x  -> resizePty pty (height x, width x)
    Nothing -> return ()

proxyShell :: IO ()
proxyShell = do
  env <- getEnvironment

  -- Make it so we can proxy characters from stdin right away
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  hSetBinaryMode stdout True

  currentSize <- size
  let newSize = case currentSize of
        Just x  -> (width x, height x)
        Nothing -> (20, 20)

  (pty, handle) <- spawnWithPty (Just env) True "bash" [] newSize

  forkIO $ pipeStdin pty handle
  forkIO $ pipeStdout pty handle
  waitForProcess handle
  return ()
