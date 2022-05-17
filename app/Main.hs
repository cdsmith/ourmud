{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad
import Data.Char (toLower)
import qualified Data.List as List
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
    hClose handle
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

      look handle playerObj ""
      clientLoop handle worldObj playerObj
    Nothing -> do
      hPutStrLn handle $ "Could not find character."
      return ()

login :: Handle -> Obj Live World -> IO (Maybe (Obj Live Player))
login _handle worldObj = atomically (lookupPlayer worldObj colinKey)

data Command = Command
  { bindings :: [String],
    description :: String,
    hide :: Bool,
    continue :: Bool,
    action :: Handle -> [String] -> Obj Live World -> Obj Live Player -> IO ()
  }

defaultCommand :: Command
defaultCommand =
  Command
    { bindings = [],
      description = "Invalid command",
      hide = False,
      continue = True,
      action = \_ _ _ _ -> return ()
    }

commands :: [Command]
commands =
  [ lookCmd,
    northCmd,
    southCmd,
    eastCmd,
    westCmd,
    goCmd,
    inventoryCmd,
    takeCmd,
    dropCmd,
    helpCmd,
    quitCmd
  ]

boundCommands :: Map String Command
boundCommands =
  Map.fromList $
    concatMap (\cmd -> [(binding, cmd) | binding <- cmd.bindings]) commands

lookCmd :: Command
lookCmd =
  defaultCommand
    { bindings = ["l", "look"],
      description = "look: Look around",
      action = \handle args _ playerObj -> look handle playerObj (unwords args)
    }

northCmd :: Command
northCmd =
  defaultCommand
    { bindings = ["n", "north"],
      description = "north: Go north",
      action = \handle _ _ playerObj -> go handle playerObj (Left North)
    }

southCmd :: Command
southCmd =
  defaultCommand
    { bindings = ["s", "south"],
      description = "south: Go south",
      action = \handle _ _ playerObj -> go handle playerObj (Left South)
    }

eastCmd :: Command
eastCmd =
  defaultCommand
    { bindings = ["e", "east"],
      description = "east: Go east",
      action = \handle _ _ playerObj -> go handle playerObj (Left East)
    }

westCmd :: Command
westCmd =
  defaultCommand
    { bindings = ["w", "west"],
      description = "west: Go west",
      action = \handle _ _ playerObj -> go handle playerObj (Left West)
    }

goCmd :: Command
goCmd =
  defaultCommand
    { bindings = ["g", "go"],
      description =
        init $ unlines
          [ "go <<way>>: Go some way",
            "  example: \"go door\" or \"go ladder\""
          ],
      action = \handle args _ playerObj -> go handle playerObj (Right (unwords args))
    }

inventoryCmd :: Command
inventoryCmd =
  defaultCommand
    { bindings = ["i", "inv", "inventory"],
      description = "inventory: Show your inventory",
      action = \handle _ _ playerObj -> inventory handle playerObj
    }

takeCmd :: Command
takeCmd =
  defaultCommand
    { bindings = ["t", "take"],
      description = "take: Take an item",
      action = \handle args _ playerObj -> takeItem handle playerObj (unwords args)
    }

dropCmd :: Command
dropCmd =
  defaultCommand
    { bindings = ["d", "drop"],
      description = "drop: Drop an item",
      action = \handle args _ playerObj -> dropItem handle playerObj (unwords args)
    }

helpCmd :: Command
helpCmd =
  defaultCommand
    { bindings = ["h", "help", "?"],
      description = "help: Show this help message",
      action = \handle _ _ _ -> do
        hPutStrLn handle "Commands:"
        forM_ commands $ \cmd -> unless cmd.hide $ do
          hPutStrLn handle cmd.description
          hPutStrLn handle $ "  commands: " ++ List.intercalate ", " cmd.bindings
    }

quitCmd :: Command
quitCmd =
  defaultCommand
    { bindings = ["q", "quit", "exit", "logout", "logoff", "lo"],
      description = "quit: Quit the game",
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
    (cmd : args) -> case Map.lookup (map toLower cmd) boundCommands of
      Nothing -> do
        hPutStrLn handle "Unknown command"
        clientLoop handle worldObj playerObj
      Just command -> do
        action command handle args worldObj playerObj
        when (continue command) (clientLoop handle worldObj playerObj)

look :: Handle -> Obj Live Player -> String -> IO ()
look handle playerObj "" = lookRoom handle playerObj
look handle playerObj "me" = lookSelf handle playerObj
look handle playerObj targetName = do
  response <- atomically $ do
    player <- readTVar playerObj.var
    room <- readTVar player.location.var
    people <- mapM (\p -> (p,) <$> readTVar p.var) room.players
    let matchingPeople =
          filter
            ( \(_, p) ->
                any
                  (== map toLower targetName)
                  (map toLower p.name : words (map toLower p.name))
            )
            people
    case matchingPeople of
      (pObj, _) : _ -> return (lookPlayer handle pObj)
      _ -> do
        items <-
          mapM
            (\i -> (i,) <$> readTVar i.var)
            (room.items ++ player.inventory)
        let matchingItems =
              filter
                ( \(_, i) ->
                    any
                      (== map toLower targetName)
                      (map toLower i.name : map (map toLower) i.nicknames)
                )
                items
        case matchingItems of
          (iObj, _) : _ -> return (lookItem handle iObj)
          _ -> do
            let matchingExits =
                  filter
                    ( \e ->
                        any
                          (== map toLower targetName)
                          (map toLower e.name : map (map toLower) e.nicknames)
                    )
                    room.exits
            case matchingExits of
              exit : _ -> return (lookExit handle exit)
              _ -> return $ do
                hPutStrLn handle $
                  "Could not find anything called " ++ targetName ++ "."
                return ()
  response

lookRoom :: Handle -> Obj Live Player -> IO ()
lookRoom handle playerObj = do
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
          "You can go " ++ map toLower (show direction) ++ " to " ++ exit.name
  forM_ people $ \p -> hPutStrLn handle $ p.name ++ " is here."
  forM_ items $ \i -> hPutStrLn handle $ "There is a " ++ i.name ++ " here."

lookSelf :: Handle -> Obj Live Player -> IO ()
lookSelf handle playerObj = do
  (player, items) <- atomically $ do
    player <- readTVar playerObj.var
    items <- mapM (\i -> readTVar i.var) player.inventory
    return (player, items)
  hPutStrLn handle $ "You are " ++ player.name ++ "."
  hPutStrLn handle player.description
  forM_ items $ \i -> hPutStrLn handle $ "You have " ++ i.name ++ "."
  return ()

lookPlayer :: Handle -> Obj Live Player -> IO ()
lookPlayer handle playerObj = do
  (player, items) <- atomically $ do
    player <- readTVar playerObj.var
    items <- mapM (\i -> readTVar i.var) player.inventory
    return (player, items)
  hPutStrLn handle $ "You see " ++ player.name ++ "."
  hPutStrLn handle player.description
  forM_ items $ \i -> hPutStrLn handle $ player.name ++ " has " ++ i.name ++ "."
  return ()

lookItem :: Handle -> Obj Live Item -> IO ()
lookItem handle itemObj = do
  item <- atomically $ readTVar itemObj.var
  hPutStrLn handle $ "You see a " ++ item.name ++ "."
  hPutStrLn handle item.description
  return ()

lookExit :: Handle -> Exit Live -> IO ()
lookExit handle exit = do
  case exit.direction of
    Nothing -> hPutStrLn handle $ "You see a " ++ exit.name ++ "."
    Just direction -> do
      hPutStrLn handle $
        "You see a "
          ++ exit.name
          ++ " to the "
          ++ map toLower (show direction)
          ++ "."

  hPutStrLn handle exit.description
  return ()

go :: Handle -> Obj Live Player -> Either Direction String -> IO ()
go handle playerObj target = do
  success <- atomically $ do
    player <- readTVar playerObj.var
    let hereObj = player.location
    here <- readTVar hereObj.var
    let directedExits = filter (matchesTarget target) here.exits
    case directedExits of
      [] -> return False
      (exit : _) -> do
        setPlayerLocation playerObj exit.destination
        return True
  case success of
    True -> look handle playerObj ""
    False -> hPutStrLn handle "You can't go that way!"
  where
    matchesTarget (Left dir) exit = exit.direction == Just dir
    matchesTarget (Right way) exit =
      map toLower way `elem` map (map toLower) (exit.name : exit.nicknames)

inventory :: Handle -> Obj Live Player -> IO ()
inventory handle playerObj = do
  items <- atomically $ do
    player <- readTVar playerObj.var
    traverse (\i -> readTVar i.var) player.inventory
  when (null items) $ hPutStrLn handle $ "You are not carrying anything."
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
