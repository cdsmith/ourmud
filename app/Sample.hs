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

  nave <-
    newNode @MUDSchema @"Room" "The Nave" $
      unlines
        [ "This is the central part of the cathedral.  There are",
          "pews all around you, and a picturesque altar at the",
          "front."
        ]

  vestibule <-
    newNode @MUDSchema @"Room" "The Vestibule" $
      unlines
        [ "This is a small antechamber just inside the cathedral.",
          "A large painting depicts the baptism of a young girl",
          "with angels surrounding her."
        ]

  doorToVestibule <-
    newNode @MUDSchema @"Exit"
      "Ornate Door"
      ["door"]
      "An ornate door stands at the cathedral entrance."
      (Just North)

  addRelated @"exit" nave doorToVestibule
  addRelated @"destination" doorToVestibule vestibule

  doorToNave <-
    newNode @MUDSchema @"Exit"
      "Ornate Door"
      ["door"]
      "An ornate door leads further into the cathedral."
      (Just South)

  addRelated @"exit" vestibule doorToNave
  addRelated @"destination" doorToNave nave

  crystal <-
    newNode @MUDSchema @"Item"
      "Corrupted Crystal"
      ["crystal"]
      "The crystal pulses with dark energy like a beating heart."

  bow <-
    newNode @MUDSchema @"Item"
      "Bow of Power"
      ["bow"]
      "This bow gives off a soft white glow."

  setRelated @"location" crystal (Just vestibule)
  setRelated @"location" bow (Just nave)

  setRelated @"start" universe nave

  return universe
