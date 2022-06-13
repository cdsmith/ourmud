{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoFieldSelectors #-}

module Sample where

import Edgy
import Model

bigBang :: Edgy MUDSchema (Node MUDSchema Universe)
bigBang = do
  universe <- getUniverse

  nave <- newNode @"Room"
  setAttribute @"name" nave "The Nave"
  setAttribute @"description" nave $
    unlines
      [ "This is the central part of the cathedral.  There are",
        "pews all around you, and a picturesque altar at the",
        "front."
      ]

  vestibule <- newNode @"Room"
  setAttribute @"name" vestibule "The Vestibule"
  setAttribute @"description" vestibule $
    unlines
      [ "This is a small antechamber just inside the cathedral.",
        "A large painting depicts the baptism of a young girl",
        "with angels surrounding her."
      ]

  doorToVestibule <- newNode @"Exit"
  setAttribute @"name" doorToVestibule "Ornate Door"
  setAttribute @"nicknames" doorToVestibule ["door"]
  setAttribute @"description"
    doorToVestibule
    "An ornate door stands at the cathedral entrance."
  setAttribute @"direction" doorToVestibule (Just North)

  addRelated @"exit" nave doorToVestibule
  addRelated @"destination" doorToVestibule vestibule

  doorToNave <- newNode @"Exit"
  setAttribute @"name" doorToNave "Ornate Door"
  setAttribute @"nicknames" doorToNave ["door"]
  setAttribute @"description"
    doorToNave
    "An ornate door leads further into the cathedral."
  setAttribute @"direction" doorToNave (Just South)

  addRelated @"exit" vestibule doorToNave
  addRelated @"destination" doorToNave nave

  crystal <- newNode @"Item"
  setAttribute @"name" crystal "Corrupted Crystal"
  setAttribute @"nicknames" crystal ["crystal"]
  setAttribute @"description"
    crystal
    "The crystal pulses with dark energy like a beating heart."

  bow <- newNode @"Item"
  setAttribute @"name" bow "Bow of Power"
  setAttribute @"nicknames" bow ["bow"]
  setAttribute @"description" bow "This bow gives off a soft white glow."

  addRelated @"contents" vestibule crystal

  colin <- newNode @"Player"
  setAttribute @"name" colin "Colin Elfwatcher"
  setAttribute @"description" colin $
    unlines
      [ "A tall and lanky figure, Colin is dressed in a rough",
        "linen tunic, and wears a short sword at his side."
      ]

  addRelated @"population" nave colin
  addRelated @"inventory" colin bow

  return universe
