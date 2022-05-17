{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad
import Data.Char (toLower)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Model
import Network.Socket
import Sample
import System.Directory (doesFileExist)
import System.IO
import World

main :: IO ()
main = do
  initialWorld <- restoreWorld
  worldObj <- instantiateWorld initialWorld

  runServer worldObj

  finalWorld <- atomically $ snapshotWorld worldObj
  writeWorld "world.json" finalWorld

runServer :: Obj Live World -> IO ()
runServer worldObj = do
  putStrLn "Starting server..."
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 4242 0)
  listen sock 2
  serverLoop sock worldObj
  close sock

serverLoop :: Socket -> Obj Live World -> IO ()
serverLoop sock worldObj = do
  (conn, _) <- accept sock
  _ <- forkIO $ do
    handle <- socketToHandle conn ReadWriteMode
    runClient handle worldObj
    close conn
  serverLoop sock worldObj

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

runClient :: Handle -> Obj Live World -> IO ()
runClient handle worldObj = do
  login handle worldObj >>= \case
    Just playerObj -> do
      player <- atomically $ readTVar playerObj.var
      hPutStrLn handle $ "Welcome, " ++ player.name

      look handle playerObj
      clientLoop handle worldObj playerObj
    Nothing -> do
      hPutStrLn handle $ "Could not find character."
      return ()

login :: Handle -> Obj Live World -> IO (Maybe (Obj Live Player))
login _handle worldObj = atomically (lookupPlayer worldObj colinKey)

data Command = Command
  { description :: String,
    hide :: Bool,
    continue :: Bool,
    action :: Handle -> [String] -> Obj Live World -> Obj Live Player -> IO ()
  }

defaultCommand :: Command
defaultCommand =
  Command
    { description = "Invalid command",
      hide = False,
      continue = True,
      action = \_ _ _ _ -> return ()
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
      action = \handle _ _ playerObj -> look handle playerObj
    }

northCmd :: Command
northCmd =
  defaultCommand
    { description =
        unlines
          [ "north: Go north",
            "  abbreviated: n"
          ],
      action = \handle _ _ playerObj -> go handle playerObj North
    }

southCmd :: Command
southCmd =
  defaultCommand
    { description =
        unlines
          [ "south: Go south",
            "  abbreviated: s"
          ],
      action = \handle _ _ playerObj -> go handle playerObj South
    }

eastCmd :: Command
eastCmd =
  defaultCommand
    { description =
        unlines
          [ "east: Go east",
            "  abbreviated: e"
          ],
      action = \handle _ _ playerObj -> go handle playerObj East
    }

westCmd :: Command
westCmd =
  defaultCommand
    { description =
        unlines
          [ "west: Go west",
            "  abbreviated: w"
          ],
      action = \handle _ _ playerObj -> go handle playerObj West
    }

inventoryCmd :: Command
inventoryCmd =
  defaultCommand
    { description =
        unlines
          [ "inventory: Show your inventory",
            "  abbreviated: inv, i"
          ],
      action = \handle _ _ playerObj -> inventory handle playerObj
    }

takeCmd :: Command
takeCmd =
  defaultCommand
    { description =
        unlines
          [ "take: Take an item",
            "  abbreviated: t, get, g"
          ],
      action = \handle args _ playerObj -> takeItem handle playerObj (unwords args)
    }

dropCmd :: Command
dropCmd =
  defaultCommand
    { description =
        unlines
          [ "drop: Drop an item",
            "  abbreviated: d, put, p"
          ],
      action = \handle args _ playerObj -> dropItem handle playerObj (unwords args)
    }

helpCmd :: Command
helpCmd =
  defaultCommand
    { description =
        unlines
          [ "help: Show this help message",
            "  abbreviated: h, ?"
          ],
      action = \handle _ _ _ -> do
        hPutStrLn handle "Commands:"
        forM_ (Map.toList commands) $ \(_, cmd) ->
          unless cmd.hide $ hPutStrLn handle cmd.description
    }

quitCmd :: Command
quitCmd =
  defaultCommand
    { description =
        unlines
          [ "quit: Quit the game",
            "  abbreviated: q, exit, lo, logout, logoff"
          ],
      action = \handle _ _ _ -> hPutStrLn handle "Goodbye!",
      continue = False
    }

clientLoop :: Handle -> Obj Live World -> Obj Live Player -> IO ()
clientLoop handle worldObj playerObj = do
  hPutStrLn handle ""
  hPutStr handle "> "
  hFlush handle
  cmdLine <- hGetLine handle

  case words cmdLine of
    [] -> clientLoop handle worldObj playerObj
    (cmd : args) -> case Map.lookup (map toLower cmd) commands of
      Nothing -> do
        hPutStrLn handle "Unknown command"
        clientLoop handle worldObj playerObj
      Just command -> do
        action command handle args worldObj playerObj
        when (continue command) (clientLoop handle worldObj playerObj)

look :: Handle -> Obj Live Player -> IO ()
look handle playerObj = do
  (room, people, items) <- atomically $ do
    player <- readTVar playerObj.var
    room <- readTVar player.location.var
    let otherPlayers = filter (\p -> p.ref /= playerObj.ref) room.players
    people <- mapM (\p -> readTVar p.var) otherPlayers
    items <- mapM (\i -> readTVar i.var) room.items
    return (room, people, items)
  hPutStrLn handle $ "You are in " ++ room.name ++ "."
  hPutStrLn handle room.description
  forM_ room.exits $ \exit -> do
    case exit.direction of
      Nothing -> return ()
      Just direction -> do
        hPutStrLn handle $
          "You can go " ++ show direction ++ " to " ++ exit.name ++ "."
  forM_ people $ \p -> hPutStrLn handle $ p.name ++ " is here."
  forM_ items $ \i -> hPutStrLn handle $ "There is a " ++ i.name ++ " here."

go :: Handle -> Obj Live Player -> Direction -> IO ()
go handle playerObj dir = do
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
    True -> look handle playerObj
    False -> hPutStrLn handle "You can't go that way!"

inventory :: Handle -> Obj Live Player -> IO ()
inventory handle playerObj = do
  items <- atomically $ do
    player <- readTVar playerObj.var
    traverse (\i -> readTVar i.var) player.inventory
  forM_ items $ \i -> hPutStrLn handle $ i.name ++ " is in your inventory."

takeItem :: Handle -> Obj Live Player -> String -> IO ()
takeItem handle playerObj itemName = do
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
    Nothing -> hPutStrLn handle "You can't find that item!"
    Just item -> hPutStrLn handle $ "You pick up " ++ item.name ++ "."

dropItem :: Handle -> Obj Live Player -> String -> IO ()
dropItem handle playerObj itemName = do
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
    Nothing -> hPutStrLn handle "You aren't carrying that item!"
    Just item -> hPutStrLn handle $ "You drop " ++ item.name ++ "."

itemGoesByName :: String -> Item l -> Bool
itemGoesByName itemName item =
  map toLower item.name == map toLower itemName
    || any (\nn -> map toLower nn == map toLower itemName) item.nicknames
