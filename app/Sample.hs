{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module Sample where

import Data.Functor.Identity
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.UUID
import Model

sampleWorld :: World Identity
sampleWorld =
  World
    { rooms =
        Identity
          ( Map.fromList
              [ ( naveKey,
                  Room
                    { guid = naveKey,
                      name = "The Nave",
                      description = "This is a room.",
                      exits =
                        Identity
                          [ Exit
                              { name = "Ornate Door",
                                direction = Just North,
                                description = "This is a door.",
                                destination = vestibuleKey
                              }
                          ],
                      items = Identity [],
                      players = Identity [playerKey]
                    }
                ),
                ( vestibuleKey,
                  Room
                    { guid = vestibuleKey,
                      name = "The Vestibule",
                      description = "This is another room.",
                      exits =
                        Identity
                          [ Exit
                              { name = "Ornate Door",
                                direction = Just South,
                                description = "This is a door.",
                                destination = naveKey
                              }
                          ],
                      items = Identity [],
                      players = Identity []
                    }
                )
              ]
          ),
      players =
        Identity
          ( Map.fromList
              [ ( playerKey,
                  Player
                    { guid = playerKey,
                      name = "Colin Elfwatcher",
                      description = "This is a player.",
                      location = Identity naveKey,
                      inventory = Identity [itemKey]
                    }
                )
              ]
          ),
      items =
        Identity
          ( Map.fromList
              [ ( itemKey,
                  Item
                    { guid = itemKey,
                      name = "Bow of Power",
                      description = "This is an item.",
                      location = Identity (Left playerKey)
                    }
                )
              ]
          )
    }

naveKey :: Ref Room
naveKey = Ref $ fromJust $ fromString "a0a0a0a0-a0a0-a0a0-a0a0-a0a0a0a0a0a0"

vestibuleKey :: Ref Room
vestibuleKey = Ref $ fromJust $ fromString "a1a1a1a1-a1a1-a1a1-a1a1-a1a1a1a1a1a1"

playerKey :: Ref Player
playerKey = Ref $ fromJust $ fromString "b0b0b0b0-b0b0-b0b0-b0b0-b0b0b0b0b0b0"

itemKey :: Ref Item
itemKey = Ref $ fromJust $ fromString "c0c0c0c0-c0c0-c0c0-c0c0-c0c0c0c0c0c0"
