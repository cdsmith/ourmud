{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Control.Concurrent.STM
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as ByteString
import Model
import Sample
import System.IO
import World

writeWorld :: FilePath -> World Snapshot -> IO ()
writeWorld path = ByteString.writeFile path . encode

readWorld :: FilePath -> IO (Maybe (World Snapshot))
readWorld path = decode <$> ByteString.readFile path

main :: IO ()
main = do
  worldObj <- instantiateWorld sampleWorld
  runClient worldObj

runClient :: Obj Live World -> IO ()
runClient worldObj = do
  playerObj <- login worldObj

  player <- atomically $ readTVar playerObj.var
  putStrLn $ "Welcome, " ++ player.name

  clientLoop worldObj playerObj

login :: Obj Live World -> IO (Obj Live Player)
login worldObj = atomically $ do
  lookupPlayer worldObj colinKey >>= \case
    Just playerObj -> return playerObj
    Nothing -> error "Player not found"

clientLoop :: Obj Live World -> Obj Live Player -> IO ()
clientLoop worldObj playerObj = do
  look playerObj
  putStr "> "
  hFlush stdout
  cmd <- getLine

  case words cmd of
    ["n"] -> go playerObj North >> clientLoop worldObj playerObj
    ["north"] -> go playerObj North >> clientLoop worldObj playerObj
    ["s"] -> go playerObj South >> clientLoop worldObj playerObj
    ["south"] -> go playerObj South >> clientLoop worldObj playerObj
    ["e"] -> go playerObj East >> clientLoop worldObj playerObj
    ["east"] -> go playerObj East >> clientLoop worldObj playerObj
    ["w"] -> go playerObj West >> clientLoop worldObj playerObj
    ["west"] -> go playerObj West >> clientLoop worldObj playerObj
    ["q"] -> quit worldObj
    ["quit"] -> quit worldObj
    _ -> do
      putStrLn "Invalid command"
      clientLoop worldObj playerObj

quit :: Obj Live World -> IO ()
quit worldObj = do
  putStrLn "Goodbye"
  world' <- atomically $ snapshotWorld worldObj
  writeWorld "world.json" world'

look :: Obj Live Player -> IO ()
look playerObj = do
  (room, people, items) <- atomically $ do
    player <- readTVar playerObj.var
    room <- readTVar player.location.var
    let otherPlayers = filter (\p -> p.ref /= playerObj.ref) room.players
    people <- mapM (\p -> readTVar p.var) otherPlayers
    items <- mapM (\i -> readTVar i.var) room.items
    return (room, people, items)
  putStrLn ""
  putStrLn $ "You are in " ++ room.name
  putStrLn $ room.description
  forM_ room.exits $ \exit -> do
    case exit.direction of
      Nothing -> return ()
      Just direction -> do
        putStrLn $ "You can go " ++ show direction ++ " to " ++ exit.name
  forM_ people $ \p -> putStrLn (p.name ++ " is here.")
  forM_ items $ \i -> putStrLn ("There is a(n) " ++ i.name ++ " here.")

go :: Obj Live Player -> Direction -> IO ()
go playerObj dir = do
  success <- atomically $ do
    player <- readTVar playerObj.var
    let hereObj = player.location
    here <- readTVar hereObj.var
    let directedExits = filter (\e -> e.direction == Just dir) here.exits
    case directedExits of
      [] -> return False
      (exit : _) -> do
        setPlayerLocation playerObj exit.destination
        return True
  unless success $ putStrLn "You can't go that way"
