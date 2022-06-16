{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Model where

import Data.Binary
import Edgy
import GHC.Generics (Generic)

data Direction = North | South | East | West deriving (Eq, Ord, Show, Generic)

instance Binary Direction

type MUDSchema =
  '[ DefNode
       (DataNode "Player")
       '[ "name" ::: String,
          "password" ::: String,
          "description" ::: String
        ],
     DefNode
       (DataNode "Room")
       '[ "name" ::: String,
          "description" ::: String
        ],
     DefNode
       (DataNode "Exit")
       '[ "name" ::: String,
          "nicknames" ::: [String],
          "description" ::: String,
          "direction" ::: Maybe Direction
        ],
     DefNode
       (DataNode "Item")
       '[ "name" ::: String,
          "nicknames" ::: [String],
          "description" ::: String
        ],
     DefDirected
       (Relation "start" One (DataNode "Room"))
       (Relation "inUniverse" One Universe),
     DefDirected
       (Relation "exit" Many (DataNode "Exit"))
       (Relation "source" One (DataNode "Room")),
     DefDirected
       (Relation "entrance" Many (DataNode "Exit"))
       (Relation "destination" One (DataNode "Room")),
     DefDirected
       (Relation "inventory" Many (DataNode "Item"))
       (Relation "owner" Optional (DataNode "Player")),
     DefDirected
       (Relation "contents" Many (DataNode "Item"))
       (Relation "location" Optional (DataNode "Room")),
     DefDirected
       (Relation "location" One (DataNode "Room"))
       (Relation "population" Many (DataNode "Player"))
   ]
