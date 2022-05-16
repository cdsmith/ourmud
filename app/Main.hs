{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Control.Concurrent.STM
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as ByteString
import Data.Functor.Identity
import qualified Data.Map.Strict as Map
import Model
import Sample
import System.IO

writeWorld :: FilePath -> World Identity -> IO ()
writeWorld path = ByteString.writeFile path . encode

readWorld :: FilePath -> IO (Maybe (World Identity))
readWorld path = decode <$> ByteString.readFile path

main :: IO ()
main = do
  world <- atomically $ instantiate sampleWorld
  runClient world playerKey

runClient :: World TVar -> Ref Player -> IO ()
runClient world playerRef = do
  playerMap <- atomically $ readTVar world.players
  case Map.lookup playerRef playerMap of
    Nothing -> do
      putStrLn "Player not found"
      return ()
    Just player -> do
      putStrLn $ "Welcome, " ++ player.name
      clientLoop world player

clientLoop :: World TVar -> Player TVar -> IO ()
clientLoop world player = do
  location <- atomically $ readTVar player.location
  roomMap <- atomically $ readTVar world.rooms
  case Map.lookup location roomMap of
    Nothing -> do
      putStrLn "Room not found"
    Just room -> do
      putStrLn ""
      putStrLn $ "You are in " ++ room.name
      putStrLn $ room.description
      exits <- atomically $ readTVar room.exits
      forM_ exits $ \exit -> do
        case exit.direction of
          Nothing -> return ()
          Just direction -> do
            putStrLn $ "You can go " ++ show direction ++ " to " ++ exit.name
      ppl <- atomically $ readTVar room.players
      playerMap <- atomically $ readTVar world.players
      forM_ ppl $ \p -> do
        case Map.lookup p playerMap of
          Nothing -> error "Player has ascendend"
          Just pp -> putStrLn (pp.name <> " is here.")
      putStr "> "
      hFlush stdout
      cmd <- getLine
      case cmd of
        "n" -> do
          go room North
          clientLoop world player
        "s" -> do
          go room South
          clientLoop world player
        "e" -> do
          go room East
          clientLoop world player
        "w" -> do
          go room West
          clientLoop world player
        "quit" -> do
          putStrLn "Goodbye"
          world' <- atomically $ snapshot world
          writeWorld "world.json" world'
        _ -> do
          putStrLn "Invalid command"
          clientLoop world player
  where
    go room dir = do
      exits <- atomically $ readTVar room.exits
      let directedExits = filter (\e -> e.direction == Just dir) exits
      case directedExits of
        [] -> do
          putStrLn "You can't go that way"
          return ()
        (exit : _) -> do
          atomically $ do
            pplHere <- readTVar room.players
            writeTVar room.players $ filter (/= player.guid) pplHere
            writeTVar player.location exit.destination
            roomMap <- readTVar world.rooms
            case Map.lookup exit.destination roomMap of
              Nothing -> error "room is gone, oh well"
              Just r -> do
                pplThere <- readTVar r.players
                writeTVar r.players (player.guid : pplThere)
                writeTVar player.location (exit.destination)
          return ()
