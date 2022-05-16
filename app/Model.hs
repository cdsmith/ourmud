{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}

module Model where

import Control.Concurrent.STM
import Data.Aeson
import Data.Bifunctor
import Data.Bitraversable
import Data.Kind (Type)
import Data.Map (Map)
import qualified Data.Map.Lazy as Map
import Data.UUID
import Control.Monad

data Liveness = Snapshot | Live

type family Mutable (l :: Liveness) (a :: Type) :: Type where
  Mutable 'Snapshot a = a
  Mutable 'Live a = TVar a

data family Obj (l :: Liveness) (a :: Liveness -> Type)

data instance Obj Snapshot a = Ref {guid :: UUID}

data instance Obj Live a = Obj {guid :: UUID, var :: TVar (a Live)}

deriving instance Show (Obj Snapshot a)

instance ToJSON (Obj Snapshot a) where
    toJSON (Ref a) = toJSON a

instance FromJSON (Obj Snapshot a) where
    parseJSON val = Ref <$> parseJSON val

data Room l = Room
  { name :: String,
    description :: String,
    exits :: [Exit l],
    items :: [Obj l Item],
    players :: [Obj l Player]
  }

deriving instance Show (Room Snapshot)

instance ToJSON (Room Snapshot) where
  toJSON room =
    object
      [ "name" .= room.name,
        "description" .= room.description,
        "exits" .= room.exits,
        "items" .= map (.guid) room.items,
        "players" .= map (.guid) room.players
      ]

instance FromJSON (Room Snapshot) where
  parseJSON = withObject "Room" $ \v ->
    Room <$> v .: "name"
      <*> v .: "description"
      <*> v .: "exits"
      <*> (v .: "items")
      <*> (v .: "players")

data Exit l = Exit
  { name :: String,
    direction :: Maybe Direction,
    description :: String,
    destination :: Obj l Room
  }

deriving instance Show (Exit Snapshot)

instance ToJSON (Exit Snapshot) where
  toJSON exit =
    object
      [ "name" .= exit.name,
        "direction" .= exit.direction,
        "description" .= exit.description,
        "destination" .= exit.destination
      ]

instance FromJSON (Exit Snapshot) where
  parseJSON = withObject "Exit" $ \v ->
    Exit <$> v .: "name"
      <*> v .: "direction"
      <*> v .: "description"
      <*> v .: "destination"

data Item l = Item
  { name :: String,
    description :: String,
    location :: Either (Obj l Player) (Obj l Room)
  }

deriving instance Show (Item Snapshot)

instance ToJSON (Item Snapshot) where
  toJSON item =
    object
      [ "name" .= item.name,
        "description" .= item.description,
        "location" .= item.location
      ]

instance FromJSON (Item Snapshot) where
  parseJSON = withObject "Item" $ \v ->
    Item <$> v .: "name"
      <*> v .: "description"
      <*> (v .: "location")

data Player l = Player
  { name :: String,
    description :: String,
    location :: Obj l Room,
    inventory :: [Obj l Item]
  }

deriving instance Show (Player Snapshot)

instance ToJSON (Player Snapshot) where
  toJSON player =
    object
      [ "name" .= player.name,
        "description" .= player.description,
        "location" .= player.location,
        "inventory" .= map (.guid) player.inventory
      ]

instance FromJSON (Player Snapshot) where
  parseJSON = withObject "Player" $ \v ->
    Player <$> v .: "name"
      <*> v .: "description"
      <*> (v .: "location")
      <*> (v .: "inventory")

data Direction = North | South | East | West deriving (Eq, Ord, Show)

instance ToJSON Direction where
  toJSON North = "north"
  toJSON South = "south"
  toJSON East = "east"
  toJSON West = "west"

instance FromJSON Direction where
  parseJSON = withText "Direction" $ \case
    "north" -> pure North
    "south" -> pure South
    "east" -> pure East
    "west" -> pure West
    _ -> fail "Invalid direction"

data World l = World
  { rooms :: Mutable l (Map UUID (Mutable l (Room l))),
    players :: Mutable l (Map UUID (Mutable l (Player l))),
    items :: Mutable l (Map UUID (Mutable l (Item l)))
  }

deriving instance Show (World Snapshot)

instance ToJSON (World Snapshot) where
  toJSON world =
    object
      [ "rooms" .= world.rooms,
        "players" .= world.players,
        "items" .= world.items
      ]

instance FromJSON (World Snapshot) where
  parseJSON = withObject "World" $ \v ->
    World <$> v .: "rooms"
      <*> v .: "players"
      <*> v .: "items"

snapshotObj :: Obj Live t -> Obj Snapshot t
snapshotObj (Obj guid _) = Ref guid

instantiateObj :: Map UUID (TVar (t Live)) -> Obj Snapshot t -> STM (Obj Live t)
instantiateObj valueMap (Ref guid) = case Map.lookup guid valueMap of
  Nothing -> error "Object not found"
  Just var -> return (Obj guid var)

class Snapshottable (t :: Liveness -> Type) where
  snapshot :: t Live -> STM (t Snapshot)
  instantiate :: World Live -> t Snapshot -> STM (t Live)

instance Snapshottable Room where
  snapshot room = do
    e <- traverse snapshot room.exits
    let i = map snapshotObj room.items
    let p = map snapshotObj room.players
    return $ room {exits = e, items = i, players = p}

  instantiate world room = do
    e <- traverse (instantiate world) room.exits
    itemMap <- readTVar world.items
    i <- traverse (instantiateObj itemMap) room.items
    playerMap <- readTVar world.players
    p <- traverse (instantiateObj playerMap) room.players
    return $ room {exits = e, items = i, players = p}

instance Snapshottable Exit where
  snapshot exit = do
    let dest = snapshotObj exit.destination
    return $ exit {destination = dest}

  instantiate world exit = do
    roomMap <- readTVar world.rooms
    dest <- instantiateObj roomMap exit.destination
    return $ exit {destination = dest}

instance Snapshottable Player where
  snapshot player = do
    let loc = snapshotObj player.location
    let inv = map snapshotObj player.inventory
    return $ player {location = loc, inventory = inv}

  instantiate world player = do
    roomMap <- readTVar world.rooms
    loc <- instantiateObj roomMap player.location
    itemMap <- readTVar world.items
    inv <- traverse (instantiateObj itemMap) player.inventory
    return $ player {location = loc, inventory = inv}

instance Snapshottable Item where
  snapshot item = do
    let loc = bimap snapshotObj snapshotObj item.location
    return $ item {location = loc}

  instantiate world item = do
    playerMap <- readTVar world.players
    roomMap <- readTVar world.rooms
    loc <-
      bitraverse
        (instantiateObj playerMap)
        (instantiateObj roomMap)
        item.location
    return $ item {location = loc}

instance Snapshottable World where
  snapshot world = do
    r <- traverse (snapshot <=< readTVar) =<< readTVar world.rooms
    p <- traverse (snapshot <=< readTVar) =<< readTVar world.players
    i <- traverse (snapshot <=< readTVar) =<< readTVar world.items
    return (World r p i)

  instantiate _ world = _