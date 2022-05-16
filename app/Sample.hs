{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Sample where

import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.UUID
import Model

sampleWorld :: World Snapshot
sampleWorld =
  World
    { rooms =
        ( Map.fromList
            [ ( naveKey,
                Room
                  { name = "The Nave",
                    description = "This is a room.",
                    exits =
                      [ Exit
                          { name = "Ornate Door",
                            direction = Just North,
                            description = "This is a door.",
                            destination = vestibuleKey
                          }
                      ],
                    items = [],
                    players = [colinKey]
                  }
              ),
              ( vestibuleKey,
                Room
                  { name = "The Vestibule",
                    description = "This is another room.",
                    exits =
                      [ Exit
                          { name = "Ornate Door",
                            direction = Just South,
                            description = "This is a door.",
                            destination = naveKey
                          }
                      ],
                    items = [crystalKey],
                    players = []
                  }
              )
            ]
        ),
      players =
        ( Map.fromList
            [ ( colinKey,
                Player
                  { name = "Colin Elfwatcher",
                    description = "This is a player.",
                    location = naveKey,
                    inventory = [bowKey]
                  }
              )
            ]
        ),
      items =
        ( Map.fromList
            [ ( bowKey,
                Item
                  { name = "Bow of Power",
                    description = "This is an item.",
                    location = (Left colinKey)
                  }
              ),
              ( crystalKey,
                Item
                  { name = "Corrupted Crystal",
                    description = "A crystal pulses with power on the ground.",
                    location = (Right vestibuleKey)
                  }
              )
            ]
        )
    }

naveKey :: Obj Snapshot Room
naveKey = Ref $ fromJust $ fromString "a0a0a0a0-a0a0-a0a0-a0a0-a0a0a0a0a0a0"

vestibuleKey :: Obj Snapshot Room
vestibuleKey = Ref $ fromJust $ fromString "a1a1a1a1-a1a1-a1a1-a1a1-a1a1a1a1a1a1"

colinKey :: Obj Snapshot Player
colinKey = Ref $ fromJust $ fromString "b0b0b0b0-b0b0-b0b0-b0b0-b0b0b0b0b0b0"

bowKey :: Obj Snapshot Item
bowKey = Ref $ fromJust $ fromString "c0c0c0c0-c0c0-c0c0-c0c0-c0c0c0c0c0c0"

crystalKey :: Obj Snapshot Item
crystalKey = Ref $ fromJust $ fromString "c1c1c1c1-c1c1-c1c1-c1c1-c1c1c1c1c1c1"
