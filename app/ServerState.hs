{-# LANGUAGE DataKinds #-}

module ServerState where

import Client (Client, closeClient)
import Control.Concurrent.STM
  ( TVar,
    atomically,
    modifyTVar,
    newTVarIO,
    readTVar,
  )
import Control.Monad (unless)
import Control.Monad.STM.Class (MonadSTM, liftSTM)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Edgy (DB, Node, NodeType (..))
import Model (MUDSchema)

data ServerState = ServerState
  { edgyDB :: DB,
    playersOnline :: TVar (Map (Node MUDSchema (DataNode "Player")) Client)
  }

newServerState :: DB -> IO ServerState
newServerState db = ServerState db <$> newTVarIO mempty

withClient ::
  ServerState ->
  Node MUDSchema (DataNode "Player") ->
  Client ->
  IO () ->
  IO Bool
withClient state player client session = do
  conflict <- atomically $ do
    conflict <- isPlayerOnline state player
    unless conflict $
      modifyTVar (playersOnline state) (Map.insert player client)
    return conflict
  unless conflict $ do
    session
    atomically $ modifyTVar (playersOnline state) $ Map.delete player
  closeClient client
  return conflict

isPlayerOnline ::
  (Monad m, MonadSTM m) =>
  ServerState ->
  Node MUDSchema (DataNode "Player") ->
  m Bool
isPlayerOnline state player =
  Map.member player <$> liftSTM (readTVar (playersOnline state))

getPlayerClient ::
  (Monad m, MonadSTM m) =>
  ServerState ->
  Node MUDSchema (DataNode "Player") ->
  m (Maybe Client)
getPlayerClient state player =
  Map.lookup player <$> liftSTM (readTVar (playersOnline state))

getAllClients :: (Monad m, MonadSTM m) => ServerState -> m [Client]
getAllClients state = Map.elems <$> liftSTM (readTVar (playersOnline state))
