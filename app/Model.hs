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

import Control.Concurrent.STM (STM, TVar, readTVar, newTVarIO)
import Data.Aeson
  ( FromJSON (parseJSON),
    FromJSONKey,
    KeyValue ((.=)),
    ToJSON (toJSON),
    ToJSONKey,
    object,
    withObject,
    withText,
    (.:),
  )
import Data.Bifunctor (Bifunctor (bimap))
import Data.Kind (Type)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import System.IO (fixIO)

data Liveness = Snapshot | Live

newtype Ref (a :: Liveness -> Type) = Ref {guid :: UUID}
  deriving newtype (Eq, Ord, Show, ToJSON, ToJSONKey, FromJSON, FromJSONKey)

data LiveObj (a :: Liveness -> Type) = Obj {ref :: Ref a, var :: TVar (a Live)}

type family Mutable (l :: Liveness) (a :: Type) :: Type where
  Mutable Snapshot a = a
  Mutable Live a = TVar a

type family Obj (l :: Liveness) (a :: Liveness -> Type) where
  Obj Snapshot a = Ref a
  Obj Live a = LiveObj a

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
  { rooms :: Map (Ref Room) (Mutable l (Room l)),
    players :: Map (Ref Player) (Mutable l (Player l)),
    items :: Map (Ref Item) (Mutable l (Item l))
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
snapshotObj (Obj ref _) = ref

instantiateObj :: Map (Ref t) (TVar (t Live)) -> Obj Snapshot t -> Obj Live t
instantiateObj valueMap ref = case Map.lookup ref valueMap of
  Nothing -> error "Object not found"
  Just var -> Obj ref var

class Snapshottable (t :: Liveness -> Type) where
  snapshot :: t Live -> t Snapshot
  instantiate :: World Live -> t Snapshot -> t Live

instance Snapshottable Room where
  snapshot room =
    let e = snapshot <$> room.exits
        i = map snapshotObj room.items
        p = map snapshotObj room.players
     in room {exits = e, items = i, players = p}

  instantiate world room =
    let e = instantiate world <$> room.exits
        i = instantiateObj world.items <$> room.items
        p = instantiateObj world.players <$> room.players
     in room {exits = e, items = i, players = p}

instance Snapshottable Exit where
  snapshot exit =
    let dest = snapshotObj exit.destination
     in exit {destination = dest}

  instantiate world exit =
    let dest = instantiateObj world.rooms exit.destination
     in exit {destination = dest}

instance Snapshottable Player where
  snapshot player =
    let loc = snapshotObj player.location
        inv = map snapshotObj player.inventory
     in player {location = loc, inventory = inv}

  instantiate world player =
    let loc = instantiateObj world.rooms player.location
        inv = instantiateObj world.items <$> player.inventory
     in player {location = loc, inventory = inv}

instance Snapshottable Item where
  snapshot item =
    let loc = bimap snapshotObj snapshotObj item.location
     in item {location = loc}

  instantiate world item =
    let loc =
          bimap
            (instantiateObj world.players)
            (instantiateObj world.rooms)
            item.location
     in item {location = loc}

snapshotWorld :: Obj Live World -> STM (World Snapshot)
snapshotWorld worldObj = do
  world <- readTVar worldObj.var
  r <- traverse (fmap snapshot . readTVar) world.rooms
  p <- traverse (fmap snapshot . readTVar) world.players
  i <- traverse (fmap snapshot . readTVar) world.items
  return (World r p i)

instantiateWorld :: World Snapshot -> IO (Obj Live World)
instantiateWorld snap = do
    world <- newTVarIO =<< fixIO go
    return (Obj (Ref UUID.nil) world)
  where
    go :: World Live -> IO (World Live)
    go world = do
        r <- traverse (newTVarIO . instantiate world) snap.rooms
        p <- traverse (newTVarIO . instantiate world) snap.players
        i <- traverse (newTVarIO . instantiate world) snap.items
        return (World {rooms = r, players = p, items = i})
