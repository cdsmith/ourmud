{-# LANGUAGE LambdaCase #-}

module Client where

import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.STM
  ( STM,
    TChan,
    TVar,
    atomically,
    newTChanIO,
    newTVarIO,
    readTChan,
    readTVar,
    retry,
    writeTChan,
    writeTVar,
  )
import Control.Monad (forever)
import Data.List.Extra (trim)
import System.IO (Handle, hClose, hFlush, hGetLine, hPutStr, hPutStrLn)

data Client = Client
  { clientHandle :: Handle,
    clientInputThread :: ThreadId,
    clientOutputThread :: ThreadId,
    clientPrompt :: TVar (Maybe Prompt),
    clientInput :: TChan String,
    clientOutput :: TChan String
  }

type Prompt = String

newClient :: Handle -> IO Client
newClient handle = do
  promptVar <- newTVarIO Nothing
  input <- newTChanIO
  output <- newTChanIO
  inputThread <- forkIO $
    forever $ do
      prompt <- atomically $ readTVar promptVar >>= maybe retry return
      hPutStr handle prompt
      hFlush handle
      result <- hGetLine handle
      atomically $ do
        writeTChan input (trim result)
        writeTVar promptVar Nothing
  outputThread <- forkIO $
    forever $ do
      (result, prompt) <-
        atomically $
          (,)
            <$> readTChan input
            <*> readTVar promptVar
      hPutStrLn handle result
      case prompt of
        Nothing -> return ()
        Just p -> do
          hPutStr handle p
          hFlush handle
  let client =
        Client
          { clientHandle = handle,
            clientInputThread = inputThread,
            clientOutputThread = outputThread,
            clientPrompt = promptVar,
            clientInput = input,
            clientOutput = output
          }
  return client

readClient :: Client -> Prompt -> IO String
readClient client prompt = do
  atomically $ do
    readTVar (clientPrompt client) >>= \case
      Just _ -> retry
      Nothing -> do
        writeTVar (clientPrompt client) (Just prompt)
  atomically $ readTChan (clientInput client)

writeClient :: Client -> String -> STM ()
writeClient client str = writeTChan (clientOutput client) str

closeClient :: Client -> IO ()
closeClient client = do
  killThread (clientInputThread client)
  killThread (clientOutputThread client)
  hClose (clientHandle client)
