{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Client (Client, closeClient, newClient, readClient, writeClient)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad
import Data.Char (toLower)
import Data.Foldable (for_)
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.TCache (atomicallySync)
import Edgy
import Model
import Network.Socket
import Sample
import System.IO

main :: IO ()
main = do
  atomicallySync $
    runEdgy @MUDSchema $ do
      universe <- getUniverse
      newWorld <- null <$> getRelated @"Room" universe
      when newWorld $ bigBang >> return ()

  runServer

runServer :: IO ()
runServer = do
  putStrLn "Starting server..."
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 4242 0)
  listen sock 2
  serverLoop sock
  close sock

serverLoop :: Socket -> IO ()
serverLoop sock = do
  (conn, _) <- accept sock
  _ <- forkIO $ do
    client <- newClient =<< socketToHandle conn ReadWriteMode
    runClient client
    closeClient client
  serverLoop sock

runClient :: Client -> IO ()
runClient client = do
  login client >>= \case
    Just player -> do
      atomically $ do
        name <- runEdgy $ getAttribute @"name" player
        writeClient client $ "Welcome, " ++ name
        runEdgy $ look client player ""
      clientLoop client player
    Nothing -> do
      atomically $ writeClient client $ "Could not find character."
      return ()

login :: Client -> IO (Maybe (Node MUDSchema (DataNode "Player")))
login client = do
  map toLower <$> readClient client "New or existing character?" >>= \case
    "new" -> do
      name <- readClient client "Name: "
      password <- readClient client "Password: "
      desc <- readClient client "Description: "
      atomicallySync $
        runEdgy $ do
          player <- newNode @MUDSchema @"Player" name password desc
          room <- getRelated @"start" =<< getUniverse
          setRelated @"location" player room
          return (Just player)
    "existing" -> do
      name <- readClient client "Name: "
      password <- readClient client "Password: "
      atomically $
        runEdgy $ do
          universe <- getUniverse
          players <- getRelated @"Player" universe
          matches <- filterM (fmap (== name) . getAttribute @"name") players
          case matches of
            [player] -> do
              correctPassword <- getAttribute @"password" player
              case password == correctPassword of
                True -> do
                  writeClient client $ "Welcome back, " ++ name
                  return (Just player)
                False -> do
                  writeClient client $ "Sorry, wrong password."
                  return Nothing
            [] -> do
              writeClient client "Sorry, no such character."
              return Nothing
            _ -> do
              writeClient client "Sorry, multiple characters with that name."
              return Nothing
    _ -> do
      atomically $ writeClient client $ "Didn't recognize your answer."
      return Nothing

data Command = Command
  { bindings :: [String],
    description :: String,
    hide :: Bool,
    continue :: Bool,
    action ::
      Client ->
      [String] ->
      Node MUDSchema (DataNode "Player") ->
      Edgy MUDSchema ()
  }

defaultCommand :: Command
defaultCommand =
  Command
    { bindings = [],
      description = "Invalid command",
      hide = False,
      continue = True,
      action = \_ _ _ -> return ()
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
      action = \client args player -> look client player (unwords args)
    }

northCmd :: Command
northCmd =
  defaultCommand
    { bindings = ["n", "north"],
      description = "north: Go north",
      action = \client _ player -> go client player (Left North)
    }

southCmd :: Command
southCmd =
  defaultCommand
    { bindings = ["s", "south"],
      description = "south: Go south",
      action = \client _ player -> go client player (Left South)
    }

eastCmd :: Command
eastCmd =
  defaultCommand
    { bindings = ["e", "east"],
      description = "east: Go east",
      action = \client _ player -> go client player (Left East)
    }

westCmd :: Command
westCmd =
  defaultCommand
    { bindings = ["w", "west"],
      description = "west: Go west",
      action = \client _ player -> go client player (Left West)
    }

goCmd :: Command
goCmd =
  defaultCommand
    { bindings = ["g", "go"],
      description =
        init $
          unlines
            [ "go <<way>>: Go some way",
              "  example: \"go door\" or \"go ladder\""
            ],
      action = \client args player -> go client player (Right (unwords args))
    }

inventoryCmd :: Command
inventoryCmd =
  defaultCommand
    { bindings = ["i", "inv", "inventory"],
      description = "inventory: Show your inventory",
      action = \client _ player -> inventory client player
    }

takeCmd :: Command
takeCmd =
  defaultCommand
    { bindings = ["t", "take"],
      description = "take: Take an item",
      action = \client args player -> takeItem client player (unwords args)
    }

dropCmd :: Command
dropCmd =
  defaultCommand
    { bindings = ["d", "drop"],
      description = "drop: Drop an item",
      action = \client args player -> dropItem client player (unwords args)
    }

helpCmd :: Command
helpCmd =
  defaultCommand
    { bindings = ["h", "help", "?"],
      description = "help: Show this help message",
      action = \client _ _ -> do
        writeClient client "Commands:"
        forM_ commands $ \cmd -> unless cmd.hide $ do
          writeClient client cmd.description
          writeClient client $
            "  commands: " ++ List.intercalate ", " cmd.bindings
    }

quitCmd :: Command
quitCmd =
  defaultCommand
    { bindings = ["q", "quit", "exit", "logout", "logoff", "lo"],
      description = "quit: Quit the game",
      action = \client _ _ -> writeClient client "Goodbye!",
      continue = False
    }

clientLoop :: Client -> Node MUDSchema (DataNode "Player") -> IO ()
clientLoop client player = do
  cmdLine <- readClient client "> "

  case words cmdLine of
    [] -> clientLoop client player
    (cmd : args) -> case Map.lookup (map toLower cmd) boundCommands of
      Nothing -> do
        atomically $ writeClient client "Unknown command"
        clientLoop client player
      Just command -> do
        atomicallySync $ runEdgy $ action command client args player
        when (continue command) (clientLoop client player)

look :: Client -> Node MUDSchema (DataNode "Player") -> String -> Edgy MUDSchema ()
look client player "" = lookRoom client player
look client player "me" = lookSelf client player
look client player targetName = do
  room <- getRelated @"location" player
  people <- getRelated @"population" room >>= filterM (named targetName)
  case people of
    person : _ -> lookPlayer client person
    _ -> do
      items <-
        (++)
          <$> getRelated @"contents" room
          <*> getRelated @"inventory" player
      matchingItems <- filterM (goesBy targetName) items
      case matchingItems of
        item : _ -> lookItem client item
        _ -> do
          exits <- getRelated @"exit" room >>= filterM (goesBy targetName)
          case exits of
            exit : _ -> lookExit client exit
            _ ->
              writeClient client $
                "Could not find anything called " ++ targetName ++ "."

lookRoom :: Client -> Node MUDSchema (DataNode "Player") -> Edgy MUDSchema ()
lookRoom client player = do
  room <- getRelated @"location" player
  writeClient client
    =<< (\n -> "You are in " ++ n ++ ".") <$> getAttribute @"name" room
  writeClient client =<< getAttribute @"description" room
  exits <- getRelated @"exit" room
  for_ exits $ \exit ->
    getAttribute @"direction" exit >>= \case
      Just dir ->
        writeClient client
          =<< ( \n ->
                  "You can go "
                    ++ map toLower (show dir)
                    ++ " to "
                    ++ n
                    ++ "."
              )
          <$> getAttribute @"name" exit
      Nothing ->
        writeClient client
          =<< (\n -> "You can go to " ++ n ++ ".")
          <$> getAttribute @"name" exit
  people <- filter (/= player) <$> getRelated @"population" room
  for_ people $ \person ->
    writeClient client
      =<< (\n -> n ++ " is here.") <$> getAttribute @"name" person
  items <- getRelated @"contents" room
  for_ items $ \item ->
    writeClient client
      =<< (\n -> "There is a " ++ n ++ " here.") <$> getAttribute @"name" item

lookSelf :: Client -> Node MUDSchema (DataNode "Player") -> Edgy MUDSchema ()
lookSelf client player = do
  writeClient client
    =<< (\n -> "You are " ++ n ++ ".") <$> getAttribute @"name" player
  writeClient client =<< getAttribute @"description" player
  items <- getRelated @"inventory" player
  for_ items $ \item ->
    writeClient client
      =<< (\n -> "You are carrying a " ++ n ++ ".")
      <$> getAttribute @"name" item

lookPlayer :: Client -> Node MUDSchema (DataNode "Player") -> Edgy MUDSchema ()
lookPlayer client player = do
  name <- getAttribute @"name" player
  writeClient client $ "You see " ++ name ++ "."
  writeClient client =<< getAttribute @"description" player
  items <- getRelated @"inventory" player
  for_ items $ \item ->
    writeClient client
      =<< (\n -> name ++ " is carrying a " ++ n ++ ".")
      <$> getAttribute @"name" item

lookItem :: Client -> Node MUDSchema (DataNode "Item") -> Edgy MUDSchema ()
lookItem client item = do
  writeClient client
    =<< (\n -> "You see " ++ n ++ ".")
    <$> getAttribute @"name" item
  writeClient client =<< getAttribute @"description" item

lookExit :: Client -> Node MUDSchema (DataNode "Exit") -> Edgy MUDSchema ()
lookExit client exit = do
  name <- getAttribute @"name" exit
  getAttribute @"direction" exit >>= \case
    Just direction ->
      writeClient client $
        "You see a "
          ++ name
          ++ " to the "
          ++ map toLower (show direction)
          ++ "."
    Nothing -> writeClient client $ "You see a " ++ name ++ "."
  writeClient client =<< getAttribute @"description" exit

go ::
  Client ->
  Node MUDSchema (DataNode "Player") ->
  Either Direction String ->
  Edgy MUDSchema ()
go client player target = do
  exits <-
    filterM (matchesTarget target)
      =<< getRelated @"exit"
      =<< getRelated @"location" player
  case exits of
    [] -> writeClient client "You can't go that way!"
    (exit : _) -> do
      destination <- getRelated @"destination" exit
      setRelated @"location" player destination
      lookRoom client player
  where
    matchesTarget ::
      Either Direction String ->
      Node MUDSchema (DataNode "Exit") ->
      Edgy MUDSchema Bool
    matchesTarget (Left dir) exit =
      (== Just dir) <$> getAttribute @"direction" exit
    matchesTarget (Right way) exit = goesBy way exit

inventory :: Client -> Node MUDSchema (DataNode "Player") -> Edgy MUDSchema ()
inventory client player = do
  items <- traverse (getAttribute @"name") =<< getRelated @"inventory" player
  when (null items) $
    writeClient client $ "You are not carrying anything."
  forM_ items $ \item ->
    writeClient client $ item ++ " is in your inventory."

takeItem ::
  Client -> Node MUDSchema (DataNode "Player") -> String -> Edgy MUDSchema ()
takeItem client player itemName = do
  items <-
    filterM (goesBy itemName)
      =<< getRelated @"contents"
      =<< getRelated @"location" player
  case items of
    [] -> writeClient client "You can't find that item!"
    (item : _) -> do
      addRelated @"inventory" player item
      clearRelated @"location" item
      writeClient client
        =<< (\n -> "You pick up " ++ n ++ ".") <$> getAttribute @"name" item

dropItem ::
  Client ->
  Node MUDSchema (DataNode "Player") ->
  String ->
  Edgy MUDSchema ()
dropItem client player itemName = do
  items <- filterM (goesBy itemName) =<< getRelated @"inventory" player
  case items of
    [] -> writeClient client "You aren't carrying that item!"
    (item : _) -> do
      removeRelated @"inventory" player item
      setRelated @"location" item =<< Just <$> getRelated @"location" player
      writeClient client
        =<< (\n -> "You drop " ++ n ++ ".") <$> getAttribute @"name" item

goesBy ::
  ( HasAttribute MUDSchema nodeType "name" ("name" ::: String),
    HasAttribute MUDSchema nodeType "nicknames" ("nicknames" ::: [String])
  ) =>
  String ->
  Node MUDSchema nodeType ->
  Edgy MUDSchema Bool
goesBy n node =
  (\name nicknames -> map toLower n `elem` map (map toLower) (name : nicknames))
    <$> getAttribute @"name" node
    <*> getAttribute @"nicknames" node

named :: String -> Node MUDSchema (DataNode "Player") -> Edgy MUDSchema Bool
named targetName person = do
  name <- getAttribute @"name" person
  return
    ( any
        (== map toLower targetName)
        (map toLower name : words (map toLower name))
    )