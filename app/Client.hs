{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Client where

import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.STM
  ( TChan,
    TVar,
    atomically,
    isEmptyTChan,
    newTChanIO,
    newTVarIO,
    readTChan,
    readTVar,
    retry,
    writeTChan,
    writeTVar,
  )
import Control.Monad (forever, unless)
import Control.Monad.STM.Class (MonadSTM (..))
import Data.List.Extra (trim)
import Edgy (Node, NodeType (..))
import Model (MUDSchema)
import System.IO (Handle, hClose, hFlush, hGetLine, hPutStr, hPutStrLn)

data Client = Client
  { clientHandle :: Handle,
    clientInputThread :: ThreadId,
    clientOutputThread :: ThreadId,
    clientPrompt :: TVar (Maybe Prompt),
    clientInput :: TChan String,
    clientOutput :: TChan String,
    clientPlayer :: TVar (Maybe (Node MUDSchema (DataNode "Player")))
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
            <$> readTChan output
            <*> readTVar promptVar
      case prompt of
        Nothing -> hPutStrLn handle result
        Just p -> do
          hPutStrLn handle ""
          hPutStrLn handle result
          hPutStr handle p
          hFlush handle
  playerVar <- newTVarIO Nothing
  let client =
        Client
          { clientHandle = handle,
            clientInputThread = inputThread,
            clientOutputThread = outputThread,
            clientPrompt = promptVar,
            clientInput = input,
            clientOutput = output,
            clientPlayer = playerVar
          }
  return client

readClient :: Client -> Prompt -> IO String
readClient client prompt = do
  atomically $ do
    outputDone <- isEmptyTChan (clientOutput client)
    unless outputDone retry
    readTVar (clientPrompt client) >>= \case
      Just _ -> retry
      Nothing -> do
        writeTVar (clientPrompt client) (Just prompt)
  atomically $ readTChan (clientInput client)

writeClient :: MonadSTM m => Client -> String -> m ()
writeClient client str = liftSTM $ writeTChan (clientOutput client) str

closeClient :: Client -> IO ()
closeClient client = do
  atomically $ do
    empty <- isEmptyTChan (clientOutput client)
    unless empty retry
  killThread (clientInputThread client)
  killThread (clientOutputThread client)
  hClose (clientHandle client)
