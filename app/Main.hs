{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Control.Concurrent.STM
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Map.Strict as Map
import Model
import Sample
import System.IO

writeWorld :: FilePath -> World Snapshot -> IO ()
writeWorld path = ByteString.writeFile path . encode

readWorld :: FilePath -> IO (Maybe (World Snapshot))
readWorld path = decode <$> ByteString.readFile path

main :: IO ()
main = do
  (world, player) <- atomically $ do
    world <- instantiate _ sampleWorld
    playerMap <- readTVar world.players
    case Map.lookup playerKey.guid playerMap of
      Nothing -> error "Player not found"
      Just player -> return (world, Obj playerKey.guid player)
  runClient world player

runClient :: World Live -> Obj Live Player -> IO ()
runClient world playerObj = do
  player <- atomically $ readTVar playerObj.var
  putStrLn $ "Welcome, " ++ player.name
  clientLoop world playerObj

clientLoop :: World Live -> Obj Live Player -> IO ()
clientLoop world playerObj = do
  look
  putStr "> "
  hFlush stdout
  cmd <- getLine

  case cmd of
    "n" -> do
      go North
      clientLoop world playerObj
    "s" -> do
      go South
      clientLoop world playerObj
    "e" -> do
      go East
      clientLoop world playerObj
    "w" -> do
      go West
      clientLoop world playerObj
    "quit" -> do
      putStrLn "Goodbye"
      world' <- atomically $ snapshot world
      writeWorld "world.json" world'
    _ -> do
      putStrLn "Invalid command"
      clientLoop world playerObj
  where
    look = do
      player <- atomically $ readTVar playerObj.var
      room <- atomically $ readTVar player.location.var
      putStrLn ""
      putStrLn $ "You are in " ++ room.name
      putStrLn $ room.description
      forM_ room.exits $ \exit -> do
        case exit.direction of
          Nothing -> return ()
          Just direction -> do
            putStrLn $ "You can go " ++ show direction ++ " to " ++ exit.name
      forM_ room.players $ \pObj -> do
        p <- atomically $ readTVar pObj.var
        putStrLn (p.name <> " is here.")
    go dir = do
      success <- atomically $ do
        player <- readTVar playerObj.var
        let hereObj = player.location
        here <- readTVar hereObj.var
        let directedExits = filter (\e -> e.direction == Just dir) here.exits
        case directedExits of
          [] -> return False
          (exit : _) -> do
            let thereObj = exit.destination
            there <- readTVar thereObj.var
            let pplHere = filter (\p -> p.guid /= playerObj.guid) here.players
            let pplThere = playerObj : there.players
            writeTVar playerObj.var ((player :: Player Live) {location = thereObj})
            writeTVar hereObj.var ((here :: Room Live) {players = pplHere})
            writeTVar thereObj.var ((there :: Room Live) {players = pplThere})
            return True
      unless success $ putStrLn "You can't go that way"
