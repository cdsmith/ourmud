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
          "direction" ::: (Maybe Direction)
        ],
     DefNode
       (DataNode "Item")
       '[ "name" ::: String,
          "nicknames" ::: [String],
          "description" ::: String
        ],
     DefDirected
       "exit"
       Many
       (DataNode "Exit")
       "source"
       One
       (DataNode "Room"),
     DefDirected
       "entrance"
       Many
       (DataNode "Exit")
       "destination"
       One
       (DataNode "Room"),
     DefDirected
       "inventory"
       Many
       (DataNode "Item")
       "owner"
       Optional
       (DataNode "Player"),
     DefDirected
       "contents"
       Many
       (DataNode "Item")
       "location"
       Optional
       (DataNode "Room"),
     DefDirected
       "location"
       One
       (DataNode "Room")
       "population"
       Many
       (DataNode "Player")
   ]