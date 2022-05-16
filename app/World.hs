{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module World where

import Control.Concurrent.STM (STM, readTVar, writeTVar)
import Control.Monad (unless)
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy as ByteString
import Model
  ( Item (..),
    LiveObj (..),
    Liveness (..),
    Obj,
    Player (..),
    Ref,
    Room (..),
    World (..),
    lookupObj,
  )

writeWorld :: FilePath -> World Snapshot -> IO ()
writeWorld path = ByteString.writeFile path . encode

readWorld :: FilePath -> IO (Maybe (World Snapshot))
readWorld path = decode <$> ByteString.readFile path

lookupPlayer :: Obj Live World -> Ref Player -> STM (Maybe (Obj Live Player))
lookupPlayer worldObj playerRef = do
  world <- readTVar worldObj.var
  return (lookupObj world.players playerRef)

lookupRoom :: Obj Live World -> Ref Room -> STM (Maybe (Obj Live Room))
lookupRoom worldObj roomRef = do
  world <- readTVar worldObj.var
  return (lookupObj world.rooms roomRef)

lookupItem :: Obj Live World -> Ref Item -> STM (Maybe (Obj Live Item))
lookupItem worldObj itemRef = do
  world <- readTVar worldObj.var
  return (lookupObj world.items itemRef)

setPlayerLocation :: Obj Live Player -> Obj Live Room -> STM ()
setPlayerLocation playerObj thereObj = do
  player <- readTVar playerObj.var
  unless (player.location == thereObj) $ do
    there <- readTVar thereObj.var
    writeTVar playerObj.var ((player :: Player Live) {location = thereObj})
    let therePlayers = playerObj : there.players
    writeTVar thereObj.var ((there :: Room Live) {players = therePlayers})
    let hereObj = player.location
    here <- readTVar hereObj.var
    let herePlayers = filter (\p -> p.ref /= playerObj.ref) here.players
    writeTVar hereObj.var ((here :: Room Live) {players = herePlayers})

pickUp :: Obj Live Player -> Obj Live Item -> STM Bool
pickUp playerObj itemObj = do
  player <- readTVar playerObj.var
  item <- readTVar itemObj.var
  if item.location == Right player.location
    then do
      here <- readTVar player.location.var
      writeTVar itemObj.var ((item :: Item Live) {location = Left playerObj})
      writeTVar
        playerObj.var
        ((player :: Player Live) {inventory = itemObj : player.inventory})
      writeTVar
        player.location.var
        ( (here :: Room Live)
            { items = filter (\i -> i.ref /= itemObj.ref) here.items
            }
        )
      return True
    else return False

putDown :: Obj Live Player -> Obj Live Item -> STM Bool
putDown playerObj itemObj = do
  player <- readTVar playerObj.var
  item <- readTVar itemObj.var
  if item.location == Left playerObj
    then do
      here <- readTVar player.location.var
      writeTVar
        itemObj.var
        ((item :: Item Live) {location = Right player.location})
      writeTVar
        playerObj.var
        ( (player :: Player Live)
            { inventory = filter (\i -> i.ref /= itemObj.ref) player.inventory
            }
        )
      writeTVar
        player.location.var
        ((here :: Room Live) {items = itemObj : here.items})
      return True
    else return False
