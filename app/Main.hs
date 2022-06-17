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

import Client
  ( Client (..),
    closeClient,
    newClient,
    readClient,
    writeClient,
  )
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.STM.Class (liftSTM)
import Data.Char (toLower)
import Data.Foldable (for_)
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.TCache (atomicallySync)
import Edgy
import Model
import Network.Socket
import Sample
import ServerState
  ( ServerState,
    getAllClients,
    getPlayerClient,
    isPlayerOnline,
    newServerState,
    withClient,
  )
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
  serverState <- newServerState
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 4242 0)
  listen sock 2
  serverLoop sock serverState
  close sock

serverLoop :: Socket -> ServerState -> IO ()
serverLoop sock serverState = do
  (conn, _) <- accept sock
  _ <- forkIO $ do
    client <- newClient =<< socketToHandle conn ReadWriteMode
    runClient serverState client
    closeClient client
  serverLoop sock serverState

runClient :: ServerState -> Client -> IO ()
runClient serverState client = do
  writeClient client "Welcome to Shae and Chris's MUD!"
  writeClient client "Type 'help' for a list of commands."
  writeClient client ""
  login client >>= \case
    Just player -> do
      conflict <- withClient serverState player client $ do
        atomically $
          runEdgy $ do
            liftSTM $ writeTVar (clientPlayer client) (Just player)
            announceToRoom serverState player (++ " magically appears.")
            look serverState client player ""
        clientLoop serverState client player
        atomically $
          runEdgy $ do
            announceToRoom serverState player (++ " magically vanishes.")
      when conflict $ do
        atomically $ writeClient client "Your character is already logged in."
    Nothing -> closeClient client

announceToRoom ::
  ServerState ->
  Node MUDSchema (DataNode "Player") ->
  (String -> String) ->
  Edgy MUDSchema ()
announceToRoom serverState player message = do
  otherPlayers <-
    filter (/= player)
      <$> (getRelated @"population" =<< getRelated @"location" player)
  name <- getAttribute @"name" player
  for_ otherPlayers $ \otherPlayer -> do
    getPlayerClient serverState otherPlayer >>= \case
      Just client -> writeClient client (message name)
      Nothing -> return ()

announceToAll ::
  ServerState ->
  Node MUDSchema (DataNode "Player") ->
  (String -> String) ->
  Edgy MUDSchema ()
announceToAll serverState player message = do
  name <- getAttribute @"name" player
  clients <-
    filterM (fmap (maybe False (/= player)) . liftSTM . readTVar . clientPlayer)
      =<< getAllClients serverState
  for_ clients $ \client -> writeClient client (message name)

login :: Client -> IO (Maybe (Node MUDSchema (DataNode "Player")))
login client = do
  name <- readClient client "Name (or \"new\"): "
  case map toLower name of
    "new" -> do
      realName <- readClient client "Name: "
      alreadyExists <- atomically $
        runEdgy $ do
          universe <- getUniverse
          getRelated @"Player" universe >>= filterM (named realName) >>= \case
            [] -> return False
            _ -> return True
      case alreadyExists of
        True -> do
          writeClient client "That name is already taken."
          login client
        False -> do
          password <- readClient client "Password: "
          confirm <- readClient client "Confirm Password: "
          case password == confirm of
            False -> do
              writeClient client "Passwords do not match."
              login client
            True -> do
              desc <- readClient client "Description: "
              atomicallySync $
                runEdgy $ do
                  player <- newNode @MUDSchema @"Player" realName password desc
                  room <- getRelated @"start" =<< getUniverse
                  setRelated @"location" player room
                  writeClient client $ "Welcome, " ++ realName ++ "!"
                  return (Just player)
    _ -> do
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
                  writeClient client $ "Welcome back, " ++ name ++ "!"
                  return (Just player)
                False -> do
                  writeClient client $ "Sorry, wrong password."
                  return Nothing
            [] -> do
              writeClient client "Sorry, no such character."
              return Nothing
            _ -> do
              writeClient client "Sorry, multiple characters with that name."
              writeClient client "Please report this as a bug."
              return Nothing

data Command = Command
  { bindings :: [String],
    description :: String,
    hide :: Bool,
    continue :: Bool,
    action ::
      ServerState ->
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
      action = \_ _ _ _ -> return ()
    }

commands :: [Command]
commands =
  [ lookCmd,
    northCmd,
    southCmd,
    eastCmd,
    westCmd,
    upCmd,
    downCmd,
    goCmd,
    inventoryCmd,
    takeCmd,
    dropCmd,
    sayCmd,
    shoutCmd,
    emoteCmd,
    whoCmd,
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
      description = "Look around",
      action = \serverState client args player ->
        look serverState client player (unwords args)
    }

northCmd :: Command
northCmd =
  defaultCommand
    { bindings = ["n", "north"],
      description = "Go north",
      action = \serverState client _ player ->
        go serverState client player (Left North)
    }

southCmd :: Command
southCmd =
  defaultCommand
    { bindings = ["s", "south"],
      description = "Go south",
      action = \serverState client _ player ->
        go serverState client player (Left South)
    }

eastCmd :: Command
eastCmd =
  defaultCommand
    { bindings = ["e", "east"],
      description = "Go east",
      action = \serverState client _ player ->
        go serverState client player (Left East)
    }

westCmd :: Command
westCmd =
  defaultCommand
    { bindings = ["w", "west"],
      description = "Go west",
      action = \serverState client _ player ->
        go serverState client player (Left West)
    }

upCmd :: Command
upCmd =
  defaultCommand
    { bindings = ["u", "up"],
      description = "Go up",
      action = \serverState client _ player ->
        go serverState client player (Left Up)
    }

downCmd :: Command
downCmd =
  defaultCommand
    { bindings = ["d", "down"],
      description = "Go down",
      action = \serverState client _ player ->
        go serverState client player (Left Down)
    }

goCmd :: Command
goCmd =
  defaultCommand
    { bindings = ["go"],
      description = "Go some way (example: \"go door\" or \"go ladder\")",
      action = \serverState client args player ->
        go serverState client player (Right (unwords args))
    }

inventoryCmd :: Command
inventoryCmd =
  defaultCommand
    { bindings = ["i", "inv", "inventory"],
      description = "Show your inventory",
      action = \_serverState client _ player -> inventory client player
    }

takeCmd :: Command
takeCmd =
  defaultCommand
    { bindings = ["t", "take", "get", "g"],
      description = "Take an item",
      action = \serverState client args player ->
        takeItem serverState client player (unwords args)
    }

dropCmd :: Command
dropCmd =
  defaultCommand
    { bindings = ["d", "dr", "drop"],
      description = "Drop an item",
      action = \serverState client args player ->
        dropItem serverState client player (unwords args)
    }

sayCmd :: Command
sayCmd =
  defaultCommand
    { bindings = ["say", "\"", "\'"],
      description = "Say something (also available with leading quotes)",
      action = \serverState _client args player ->
        say serverState player (unwords args)
    }

shoutCmd :: Command
shoutCmd =
  defaultCommand
    { bindings = ["shout", "sh"],
      description = "Shout something, so everyone can hear",
      action = \serverState _client args player ->
        shout serverState player (unwords args)
    }

whoCmd :: Command
whoCmd =
  defaultCommand
    { bindings = ["who"],
      description = "List everyone who is online",
      action = \serverState client _args player ->
        who serverState client player
    }

emoteCmd :: Command
emoteCmd =
  defaultCommand
    { bindings = ["emote", ":"],
      description = "Emote something",
      action = \serverState _client args player ->
        emote serverState player (unwords args)
    }

helpCmd :: Command
helpCmd =
  defaultCommand
    { bindings = ["h", "help", "?"],
      description = "Show this help message",
      action = \_serverState client _ _ -> do
        writeClient client "Commands:"
        forM_ commands $ \cmd -> unless cmd.hide $ do
          writeClient client $ "  * " ++ List.intercalate ", " cmd.bindings
          writeClient client $ "    " ++ cmd.description
    }

quitCmd :: Command
quitCmd =
  defaultCommand
    { bindings = ["q", "quit", "exit", "logout"],
      description = "Quit the game",
      action = \_serverState client _ _ -> writeClient client "Goodbye!",
      continue = False
    }

clientLoop ::
  ServerState ->
  Client ->
  Node MUDSchema (DataNode "Player") ->
  IO ()
clientLoop serverState client player = do
  cmdLine <- readClient client "> "
  let fixedCmd = case cmdLine of
        '"' : rest -> "say " ++ rest
        '\'' : rest -> "say " ++ rest
        ':' : rest -> "emote " ++ rest
        other -> other

  case words fixedCmd of
    [] -> clientLoop serverState client player
    (cmd : args) -> case Map.lookup (map toLower cmd) boundCommands of
      Nothing -> do
        atomically $ writeClient client "Unknown command"
        clientLoop serverState client player
      Just command -> do
        atomicallySync $ runEdgy $ action command serverState client args player
        when (continue command) (clientLoop serverState client player)

look ::
  ServerState ->
  Client ->
  Node MUDSchema (DataNode "Player") ->
  String ->
  Edgy MUDSchema ()
look serverState client player "" = lookRoom serverState client player
look _ client player "me" = lookSelf client player
look serverState client player targetName = do
  room <- getRelated @"location" player
  people <-
    getRelated @"population" room
      >>= filterM (named targetName)
      >>= filterM (isPlayerOnline serverState)
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

lookRoom ::
  ServerState ->
  Client ->
  Node MUDSchema (DataNode "Player") ->
  Edgy MUDSchema ()
lookRoom serverState client player = do
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
                    ++ " via "
                    ++ n
                    ++ "."
              )
          <$> getAttribute @"name" exit
      Nothing ->
        writeClient client
          =<< (\n -> "You can go to " ++ n ++ ".")
          <$> getAttribute @"name" exit
  people <-
    filter (/= player) <$> getRelated @"population" room
      >>= filterM (isPlayerOnline serverState)
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
  ServerState ->
  Client ->
  Node MUDSchema (DataNode "Player") ->
  Either Direction String ->
  Edgy MUDSchema ()
go serverState client player target = do
  exits <-
    filterM (matchesTarget target)
      =<< getRelated @"exit"
      =<< getRelated @"location" player
  case exits of
    [] -> writeClient client "You can't go that way!"
    (exit : _) -> do
      destination <- getRelated @"destination" exit
      announceToRoom serverState player (++ " has left.")
      setRelated @"location" player destination
      announceToRoom serverState player (++ " has arrived.")
      lookRoom serverState client player
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
  ServerState ->
  Client ->
  Node MUDSchema (DataNode "Player") ->
  String ->
  Edgy MUDSchema ()
takeItem serverState client player target = do
  items <-
    filterM (goesBy target)
      =<< getRelated @"contents"
      =<< getRelated @"location" player
  case items of
    [] -> writeClient client "You can't find that item!"
    (item : _) -> do
      addRelated @"inventory" player item
      clearRelated @"location" item
      itemName <- getAttribute @"name" item
      announceToRoom serverState player (++ " has taken " ++ itemName ++ ".")
      writeClient client
        =<< (\n -> "You pick up " ++ n ++ ".") <$> getAttribute @"name" item

dropItem ::
  ServerState ->
  Client ->
  Node MUDSchema (DataNode "Player") ->
  String ->
  Edgy MUDSchema ()
dropItem serverState client player target = do
  items <- filterM (goesBy target) =<< getRelated @"inventory" player
  case items of
    [] -> writeClient client "You aren't carrying that item!"
    (item : _) -> do
      removeRelated @"inventory" player item
      setRelated @"location" item =<< Just <$> getRelated @"location" player
      itemName <- getAttribute @"name" item
      announceToRoom serverState player (++ " has dropped " ++ itemName ++ ".")
      writeClient client
        =<< (\n -> "You drop " ++ n ++ ".") <$> getAttribute @"name" item

say ::
  ServerState ->
  Node MUDSchema (DataNode "Player") ->
  String ->
  Edgy MUDSchema ()
say serverState player message =
  announceToRoom serverState player (++ ": " ++ message)

emote ::
  ServerState ->
  Node MUDSchema (DataNode "Player") ->
  String ->
  Edgy MUDSchema ()
emote serverState player message =
  announceToRoom serverState player (\n -> "* " ++ n ++ " " ++ message)

shout ::
  ServerState ->
  Node MUDSchema ('DataNode "Player") ->
  String ->
  Edgy MUDSchema ()
shout serverState player message =
  announceToAll serverState player (++ " shouts: " ++ message)

who ::
  ServerState ->
  Client ->
  Node MUDSchema ('DataNode "Player") ->
  Edgy MUDSchema ()
who serverState client player = do
  players <-
    filter (/= player) . catMaybes
      <$> ( traverse (liftSTM . readTVar . clientPlayer)
              =<< getAllClients serverState
          )
  for_ players $ writeClient client <=< getAttribute @"name"

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