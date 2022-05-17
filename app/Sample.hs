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
                    description =
                      unlines
                        [ "This is the central part of the cathedral.  There are",
                          "pews all around you, and a picturesque altar at the",
                          "front."
                        ],
                    exits =
                      [ Exit
                          { name = "Ornate Door",
                            nicknames = ["door"],
                            direction = Just North,
                            description = "An ornate door stands at the cathedral entrance.",
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
                    description =
                      unlines
                        [ "This is a small antechamber just inside the cathedral.",
                          "A large painting depicts the baptism of a young girl",
                          "with angels surrounding her."
                        ],
                    exits =
                      [ Exit
                          { name = "Ornate Door",
                            nicknames = ["door"],
                            direction = Just South,
                            description = "An ornate door leads further into the cathedral.",
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
                    description =
                      unlines
                        [ "A tall and lanky figure, Colin is dressed in a rough",
                          "linen tunic, and wears a short sword at his side."
                        ],
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
                    nicknames = ["bow"],
                    description = "This bow gives off a soft white glow.",
                    location = (Left colinKey)
                  }
              ),
              ( crystalKey,
                Item
                  { name = "Corrupted Crystal",
                    nicknames = ["crystal"],
                    description = "The crystal pulses with dark energy like a beating heart.",
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
