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
              [ ( Ref naveKey,
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
                                destination = Ref vestibuleKey
                              }
                          ],
                      items = Identity [],
                      players = Identity [Ref playerKey]
                    }
                ),
                ( Ref vestibuleKey,
                  Room
                    { guid = vestibuleKey,
                      name = "The Nave",
                      description = "This is a room.",
                      exits =
                        Identity
                          [ Exit
                              { name = "Ornate Door",
                                direction = Just South,
                                description = "This is a door.",
                                destination = Ref naveKey
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
              [ ( Ref playerKey,
                  Player
                    { guid = playerKey,
                      name = "Colin Elfwatcher",
                      description = "This is a player.",
                      location = Identity (Just (Ref naveKey)),
                      inventory = Identity [Ref itemKey]
                    }
                )
              ]
          ),
      items =
        Identity
          ( Map.fromList
              [ ( Ref itemKey,
                  Item
                    { guid = itemKey,
                      name = "Bow of Power",
                      description = "This is an item.",
                      location = Identity (Left (Ref playerKey))
                    }
                )
              ]
          )
    }
  where
    naveKey = fromJust $ fromString "a0a0a0a0-a0a0-a0a0-a0a0-a0a0a0a0a0a0"
    vestibuleKey = fromJust $ fromString "a1a1a1a1-a1a1-a1a1-a1a1-a1a1a1a1a1a1"
    playerKey = fromJust $ fromString "b0b0b0b0-b0b0-b0b0-b0b0-b0b0b0b0b0b0"
    itemKey = fromJust $ fromString "c0c0c0c0-c0c0-c0c0-c0c0-c0c0c0c0c0c0"
