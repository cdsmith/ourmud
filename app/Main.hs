{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Control.Concurrent.STM
import Control.Monad
import Data.Char (toLower)
import Model
import Sample
import System.Directory (doesFileExist)
import System.IO
import World

main :: IO ()
main = do
  initialWorld <- restoreWorld
  worldObj <- instantiateWorld initialWorld

  runClient worldObj

  finalWorld <- atomically $ snapshotWorld worldObj
  writeWorld "world.json" finalWorld

restoreWorld :: IO (World Snapshot)
restoreWorld = do
  exists <- doesFileExist "world.json"
  if exists
    then do
      readWorld "world.json" >>= \case
        Just world -> do
          putStrLn "Successfully restored the world."
          return world
        Nothing -> do
          putStrLn "Could not read world.json.  Using the sample world instead!"
          return sampleWorld
    else do
      putStrLn "No saved world.  Using the sample world instead!"
      return sampleWorld

runClient :: Obj Live World -> IO ()
runClient worldObj = do
  playerObj <- login worldObj

  player <- atomically $ readTVar playerObj.var
  putStrLn $ "Welcome, " ++ player.name

  look playerObj
  clientLoop worldObj playerObj

login :: Obj Live World -> IO (Obj Live Player)
login worldObj = atomically $ do
  lookupPlayer worldObj colinKey >>= \case
    Just playerObj -> return playerObj
    Nothing -> error "Player not found"

clientLoop :: Obj Live World -> Obj Live Player -> IO ()
clientLoop worldObj playerObj = do
  putStrLn ""
  putStr "> "
  hFlush stdout
  cmd <- getLine

  let continue = clientLoop worldObj playerObj
  case words cmd of
    [] -> continue
    ["l"] -> look playerObj >> continue
    ["look"] -> look playerObj >> continue
    ["n"] -> go playerObj North >> continue
    ["north"] -> go playerObj North >> continue
    ["s"] -> go playerObj South >> continue
    ["south"] -> go playerObj South >> continue
    ["e"] -> go playerObj East >> continue
    ["east"] -> go playerObj East >> continue
    ["w"] -> go playerObj West >> continue
    ["west"] -> go playerObj West >> continue
    ["i"] -> inventory playerObj >> continue
    ["inv"] -> inventory playerObj >> continue
    ["inventory"] -> inventory playerObj >> continue
    ("g" : item) -> takeItem playerObj (unwords item) >> continue
    ("get" : item) -> takeItem playerObj (unwords item) >> continue
    ("t" : item) -> takeItem playerObj (unwords item) >> continue
    ("take" : item) -> takeItem playerObj (unwords item) >> continue
    ("pick" : "up" : item) -> takeItem playerObj (unwords item) >> continue
    ("d" : item) -> dropItem playerObj (unwords item) >> continue
    ("drop" : item) -> dropItem playerObj (unwords item) >> continue
    ("p" : item) -> dropItem playerObj (unwords item) >> continue
    ("put" : item) -> dropItem playerObj (unwords item) >> continue
    ["q"] -> putStrLn "Goodbye!"
    ["quit"] -> putStrLn "Goodbye!"
    _ -> do
      putStrLn "Invalid command"
      clientLoop worldObj playerObj

look :: Obj Live Player -> IO ()
look playerObj = do
  (room, people, items) <- atomically $ do
    player <- readTVar playerObj.var
    room <- readTVar player.location.var
    let otherPlayers = filter (\p -> p.ref /= playerObj.ref) room.players
    people <- mapM (\p -> readTVar p.var) otherPlayers
    items <- mapM (\i -> readTVar i.var) room.items
    return (room, people, items)
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
  case success of
    True -> look playerObj
    False -> putStrLn "You can't go that way!"

inventory :: Obj Live Player -> IO ()
inventory playerObj = do
  items <- atomically $ do
    player <- readTVar playerObj.var
    traverse (\i -> readTVar i.var) player.inventory
  forM_ items $ \i -> putStrLn (i.name ++ " is in your inventory.")

takeItem :: Obj Live Player -> String -> IO ()
takeItem playerObj itemName = do
  result <- atomically $ do
    player <- readTVar playerObj.var
    let hereObj = player.location
    here <- readTVar hereObj.var
    items <- zip here.items <$> traverse (\i -> readTVar i.var) here.items
    let matches = filter (itemGoesByName itemName . snd) items
    case matches of
      [] -> return Nothing
      ((itemObj, item) : _) -> do
        success <- pickUp playerObj itemObj
        if success then return (Just item) else return Nothing
  case result of
    Nothing -> putStrLn "You can't find that item!"
    Just item -> putStrLn ("You pick up " ++ item.name)

dropItem :: Obj Live Player -> String -> IO ()
dropItem playerObj itemName = do
  result <- atomically $ do
    player <- readTVar playerObj.var
    items <- zip player.inventory <$> traverse (\i -> readTVar i.var) player.inventory
    let matches = filter (itemGoesByName itemName . snd) items
    case matches of
      [] -> return Nothing
      ((itemObj, item) : _) -> do
        success <- putDown playerObj itemObj
        if success then return (Just item) else return Nothing
  case result of
    Nothing -> putStrLn "You aren't carrying that item!"
    Just item -> putStrLn ("You drop " ++ item.name)

itemGoesByName :: String -> Item l -> Bool
itemGoesByName itemName item =
  map toLower item.name == map toLower itemName
    || any (\nn -> map toLower nn == map toLower itemName) item.nicknames