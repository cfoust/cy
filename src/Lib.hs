module Lib
  ( proxyShell
  )
where

import           Control.Concurrent             ( forkIO )
import           Control.Monad
import           System.Environment
import           System.Exit
import           System.Signal                  ( installHandler
                                                , Signal
                                                )
import           System.Posix.Signals.Exts      ( sigWINCH )
import           System.IO
import           System.Posix.Pty
import           System.Process
import qualified Data.ByteString.Char8         as C
import           System.Console.Terminal.Size   ( size
                                                , height
                                                , width
                                                )
import           Control.Concurrent.Chan


data Mutation = Mutation Bool C.ByteString

handleMutation :: Chan Mutation -> IO ()
handleMutation channel = do
  next <- readChan channel
  handleMutation channel

-- |Check whether the process referenced by `handle` has exited.
isDone :: ProcessHandle -> IO Bool
isDone handle = do
  code <- getProcessExitCode handle
  return (code /= Nothing)

-- |Pass any bytes we get on stdin down to the pseudo-tty.
pipeStdin :: Pty -> Chan Mutation -> ProcessHandle -> IO ()
pipeStdin pty channel process = do
  done <- isDone process
  if done
    then return ()
    else do
      haveInput <- hWaitForInput stdin 200
      when haveInput $ do
        char <- hGetChar stdin
        writePty pty (C.singleton char)
      pipeStdin pty channel process

-- |Pass any bytes we get from the pseudo-tty's stdout back to the actual
-- |stdout.
pipeStdout :: Pty -> Chan Mutation -> ProcessHandle -> IO ()
pipeStdout pty channel process = do
  done <- isDone process
  if done
    then return ()
    else do
      threadWaitReadPty pty
      chars <- readPty pty
      C.hPut stdout chars
      writeChan channel (Mutation True chars)
      pipeStdout pty channel process

-- |Signal handler for SIGWINCH.
handleSize :: Pty -> Signal -> IO ()
handleSize pty signal = do
  winSize <- size
  case winSize of
    Just x  -> resizePty pty (height x, width x)
    Nothing -> return ()

-- |Spawn a new shell process and proxy its stdin/stdout.
proxyShell :: IO ()
proxyShell = do
  env <- getEnvironment

  -- Make it so we can proxy characters from stdin right away
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  hSetBinaryMode stdout True

  currentSize <- size
  -- This is just a default because for some reason size might not be defined.
  let newSize = case currentSize of
        Just x  -> (width x, height x)
        Nothing -> (20, 20)

  (pty, process) <- spawnWithPty (Just env) True "bash" [] newSize

  installHandler sigWINCH $ handleSize pty

  dataChan <- newChan

  forkIO $ handleMutation dataChan
  forkIO $ pipeStdin pty dataChan process
  forkIO $ pipeStdout pty dataChan process
  waitForProcess process
  return ()
