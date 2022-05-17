{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Control.Concurrent.STM
import Control.Monad
import Data.Char (toLower)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
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

data Command = Command
  { description :: String,
    hide :: Bool,
    continue :: Bool,
    action :: [String] -> Obj Live World -> Obj Live Player -> IO ()
  }

defaultCommand :: Command
defaultCommand =
  Command
    { description = "Invalid command",
      hide = False,
      continue = True,
      action = \_ _ _ -> return ()
    }

commands :: Map String Command
commands =
  Map.fromList
    [ ("l", lookCmd {hide = True}),
      ("look", lookCmd),
      ("n", northCmd {hide = True}),
      ("north", northCmd),
      ("s", southCmd {hide = True}),
      ("south", southCmd),
      ("e", eastCmd {hide = True}),
      ("east", eastCmd),
      ("w", westCmd {hide = True}),
      ("west", westCmd),
      ("i", inventoryCmd {hide = True}),
      ("inv", inventoryCmd {hide = True}),
      ("inventory", inventoryCmd),
      ("g", takeCmd {hide = True}),
      ("get", takeCmd {hide = True}),
      ("t", takeCmd {hide = True}),
      ("take", takeCmd),
      ("p", dropCmd {hide = True}),
      ("put", dropCmd {hide = True}),
      ("d", dropCmd {hide = True}),
      ("drop", dropCmd),
      ("help", helpCmd),
      ("h", helpCmd {hide = True}),
      ("?", helpCmd {hide = True}),
      ("q", quitCmd {hide = True}),
      ("exit", quitCmd {hide = True}),
      ("lo", quitCmd {hide = True}),
      ("logout", quitCmd {hide = True}),
      ("logoff", quitCmd {hide = True}),
      ("quit", quitCmd)
    ]

lookCmd :: Command
lookCmd =
  defaultCommand
    { description =
        unlines
          [ "look: Look around",
            "  abbreviated: l"
          ],
      action = \_ _ playerObj -> look playerObj
    }

northCmd :: Command
northCmd =
  defaultCommand
    { description =
        unlines
          [ "north: Go north",
            "  abbreviated: n"
          ],
      action = \_ _ playerObj -> go playerObj North
    }

southCmd :: Command
southCmd =
  defaultCommand
    { description =
        unlines
          [ "south: Go south",
            "  abbreviated: s"
          ],
      action = \_ _ playerObj -> go playerObj South
    }

eastCmd :: Command
eastCmd =
  defaultCommand
    { description =
        unlines
          [ "east: Go east",
            "  abbreviated: e"
          ],
      action = \_ _ playerObj -> go playerObj East
    }

westCmd :: Command
westCmd =
  defaultCommand
    { description =
        unlines
          [ "west: Go west",
            "  abbreviated: w"
          ],
      action = \_ _ playerObj -> go playerObj West
    }

inventoryCmd :: Command
inventoryCmd =
  defaultCommand
    { description =
        unlines
          [ "inventory: Show your inventory",
            "  abbreviated: inv, i"
          ],
      action = \_ _ playerObj -> inventory playerObj
    }

takeCmd :: Command
takeCmd =
  defaultCommand
    { description =
        unlines
          [ "take: Take an item",
            "  abbreviated: t, get, g"
          ],
      action = \args _ playerObj -> takeItem playerObj (unwords args)
    }

dropCmd :: Command
dropCmd =
  defaultCommand
    { description =
        unlines
          [ "drop: Drop an item",
            "  abbreviated: d, put, p"
          ],
      action = \args _ playerObj -> dropItem playerObj (unwords args)
    }

helpCmd :: Command
helpCmd =
  defaultCommand
    { description =
        unlines
          [ "help: Show this help message",
            "  abbreviated: h, ?"
          ],
      action = \_ _ _ -> do
        putStrLn "Commands:"
        forM_ (Map.toList commands) $ \(_, cmd) ->
          unless cmd.hide $ putStrLn cmd.description
        return ()
    }

quitCmd :: Command
quitCmd =
  defaultCommand
    { description =
        unlines
          [ "quit: Quit the game",
            "  abbreviated: q, exit, lo, logout, logoff"
          ],
      action = \_ _ _ -> putStrLn "Goodbye!",
      continue = False
    }

clientLoop :: Obj Live World -> Obj Live Player -> IO ()
clientLoop worldObj playerObj = do
  putStrLn ""
  putStr "> "
  hFlush stdout
  cmdLine <- getLine

  case words cmdLine of
    [] -> clientLoop worldObj playerObj
    (cmd : args) -> case Map.lookup (map toLower cmd) commands of
      Nothing -> do
        putStrLn "Unknown command"
        clientLoop worldObj playerObj
      Just command -> do
        action command args worldObj playerObj
        when (continue command) (clientLoop worldObj playerObj)

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