{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NoFieldSelectors #-}

module Model where

import Control.Concurrent.STM
import Data.Aeson
import Data.Functor.Identity
import Data.Kind (Type)
import Data.Map (Map)
import Data.UUID

newtype Ref (a :: (Type -> Type) -> Type) = Ref {guid :: UUID}
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToJSON, FromJSON, ToJSONKey, FromJSONKey)

data Room f = Room
  { guid :: UUID,
    name :: String,
    description :: String,
    exits :: f [Exit],
    items :: f [Ref Item],
    players :: f [Ref Player]
  }

deriving instance Show (Room Identity)

instance ToJSON (Room Identity) where
  toJSON room =
    object
      [ "guid" .= room.guid,
        "name" .= room.name,
        "description" .= room.description,
        "exits" .= room.exits,
        "items" .= map (.guid) (runIdentity room.items),
        "players" .= map (.guid) (runIdentity room.players)
      ]

instance FromJSON (Room Identity) where
  parseJSON = withObject "Room" $ \v ->
    Room <$> v .: "guid"
      <*> v .: "name"
      <*> v .: "description"
      <*> v .: "exits"
      <*> (Identity <$> v .: "items")
      <*> (Identity <$> v .: "players")

data Exit = Exit
  { name :: String,
    direction :: Maybe Direction,
    description :: String,
    destination :: Ref Room
  }
  deriving (Show)

instance ToJSON Exit where
  toJSON exit =
    object
      [ "name" .= exit.name,
        "direction" .= exit.direction,
        "description" .= exit.description,
        "destination" .= exit.destination
      ]

instance FromJSON Exit where
  parseJSON = withObject "Exit" $ \v ->
    Exit <$> v .: "name"
      <*> v .: "direction"
      <*> v .: "description"
      <*> v .: "destination"

data Item f = Item
  { guid :: UUID,
    name :: String,
    description :: String,
    location :: f (Either (Ref Player) (Ref Room))
  }

deriving instance Show (Item Identity)

instance ToJSON (Item Identity) where
  toJSON item =
    object
      [ "guid" .= item.guid,
        "name" .= item.name,
        "description" .= item.description,
        "location" .= runIdentity item.location
      ]

instance FromJSON (Item Identity) where
  parseJSON = withObject "Item" $ \v ->
    Item <$> v .: "guid"
      <*> v .: "name"
      <*> v .: "description"
      <*> (Identity <$> v .: "location")

data Player f = Player
  { guid :: UUID,
    name :: String,
    description :: String,
    location :: f (Maybe (Ref Room)),
    inventory :: f [Ref Item]
  }

deriving instance Show (Player Identity)

instance ToJSON (Player Identity) where
  toJSON player =
    object
      [ "guid" .= player.guid,
        "name" .= player.name,
        "description" .= player.description,
        "location" .= runIdentity player.location,
        "inventory" .= map (.guid) (runIdentity player.inventory)
      ]

instance FromJSON (Player Identity) where
  parseJSON = withObject "Player" $ \v ->
    Player <$> v .: "guid"
      <*> v .: "name"
      <*> v .: "description"
      <*> (Identity <$> v .: "location")
      <*> (Identity <$> v .: "inventory")

data Direction = North | South | East | West deriving (Show)

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

data World f = World
  { rooms :: f (Map (Ref Room) (Room f)),
    players :: f (Map (Ref Player) (Player f)),
    items :: f (Map (Ref Item) (Item f))
  }

deriving instance Show (World Identity)

instance ToJSON (World Identity) where
  toJSON world =
    object
      [ "rooms" .= runIdentity world.rooms,
        "players" .= runIdentity world.players,
        "items" .= runIdentity world.items
      ]

instance FromJSON (World Identity) where
  parseJSON = withObject "World" $ \v ->
    World <$> v .: "rooms"
      <*> v .: "players"
      <*> v .: "items"

class Snapshottable (t :: (Type -> Type) -> Type) where
  snapshot :: t TVar -> STM (t Identity)
  instantiate :: t Identity -> STM (t TVar)

instance Snapshottable Room where
  snapshot room = do
    e <- Identity <$> readTVar room.exits
    i <- Identity <$> readTVar room.items
    p <- Identity <$> readTVar room.players
    return $ room {exits = e, items = i, players = p}

  instantiate room = do
    e <- newTVar (runIdentity room.exits)
    i <- newTVar (runIdentity room.items)
    p <- newTVar (runIdentity room.players)
    return $ room {exits = e, items = i, players = p}

instance Snapshottable Player where
  snapshot player = do
    l <- Identity <$> readTVar player.location
    i <- Identity <$> readTVar player.inventory
    return $ player {location = l, inventory = i}

  instantiate player = do
    l <- newTVar (runIdentity player.location)
    i <- newTVar (runIdentity player.inventory)
    return $ player {location = l, inventory = i}

instance Snapshottable Item where
  snapshot item = do
    l <- Identity <$> readTVar item.location
    return $ item {location = l}

  instantiate item = do
    l <- newTVar (runIdentity item.location)
    return $ item {location = l}

instance Snapshottable World where
  snapshot world = do
    r <- Identity <$> (traverse snapshot =<< readTVar world.rooms)
    p <- Identity <$> (traverse snapshot =<< readTVar world.players)
    i <- Identity <$> (traverse snapshot =<< readTVar world.items)
    return (World r p i)

  instantiate world = do
    r <- newTVar =<< traverse instantiate (runIdentity world.rooms)
    p <- newTVar =<< traverse instantiate (runIdentity world.players)
    i <- newTVar =<< traverse instantiate (runIdentity world.items)
    return (World r p i)
