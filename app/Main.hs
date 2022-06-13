{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad
import Data.Char (toLower)
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
    handle <- socketToHandle conn ReadWriteMode
    runClient handle
    hClose handle
  serverLoop sock

runClient :: Handle -> IO ()
runClient handle = do
  login handle >>= \case
    Just player -> do
      name <- atomically $ runEdgy $ getAttribute @"name" player
      hPutStrLn handle $ "Welcome, " ++ name

      look handle player ""
      clientLoop handle player
    Nothing -> do
      hPutStrLn handle $ "Could not find character."
      return ()

login :: Handle -> IO (Maybe (Node MUDSchema (DataNode "Player")))
login _handle = atomically $
  runEdgy @MUDSchema $ do
    universe <- getUniverse
    players <- getRelated @"Player" universe
    matches <- filterM (fmap (== "Colin Elfwatcher") . getAttribute @"name") players
    case matches of
      [player] -> return (Just player)
      _ -> return Nothing

data Command = Command
  { bindings :: [String],
    description :: String,
    hide :: Bool,
    continue :: Bool,
    action :: Handle -> [String] -> Node MUDSchema (DataNode "Player") -> IO ()
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
      action = \handle args player -> look handle player (unwords args)
    }

northCmd :: Command
northCmd =
  defaultCommand
    { bindings = ["n", "north"],
      description = "north: Go north",
      action = \handle _ player -> go handle player (Left North)
    }

southCmd :: Command
southCmd =
  defaultCommand
    { bindings = ["s", "south"],
      description = "south: Go south",
      action = \handle _ player -> go handle player (Left South)
    }

eastCmd :: Command
eastCmd =
  defaultCommand
    { bindings = ["e", "east"],
      description = "east: Go east",
      action = \handle _ player -> go handle player (Left East)
    }

westCmd :: Command
westCmd =
  defaultCommand
    { bindings = ["w", "west"],
      description = "west: Go west",
      action = \handle _ player -> go handle player (Left West)
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
      action = \handle args player -> go handle player (Right (unwords args))
    }

inventoryCmd :: Command
inventoryCmd =
  defaultCommand
    { bindings = ["i", "inv", "inventory"],
      description = "inventory: Show your inventory",
      action = \handle _ player -> inventory handle player
    }

takeCmd :: Command
takeCmd =
  defaultCommand
    { bindings = ["t", "take"],
      description = "take: Take an item",
      action = \handle args player -> takeItem handle player (unwords args)
    }

dropCmd :: Command
dropCmd =
  defaultCommand
    { bindings = ["d", "drop"],
      description = "drop: Drop an item",
      action = \handle args player -> dropItem handle player (unwords args)
    }

helpCmd :: Command
helpCmd =
  defaultCommand
    { bindings = ["h", "help", "?"],
      description = "help: Show this help message",
      action = \handle _ _ -> do
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
      action = \handle _ _ -> hPutStrLn handle "Goodbye!",
      continue = False
    }

clientLoop :: Handle -> Node MUDSchema (DataNode "Player") -> IO ()
clientLoop handle player = do
  hPutStrLn handle ""
  hPutStr handle "> "
  hFlush handle
  cmdLine <- hGetLine handle

  case words cmdLine of
    [] -> clientLoop handle player
    (cmd : args) -> case Map.lookup (map toLower cmd) boundCommands of
      Nothing -> do
        hPutStrLn handle "Unknown command"
        clientLoop handle player
      Just command -> do
        action command handle args player
        when (continue command) (clientLoop handle player)

look :: Handle -> Node MUDSchema (DataNode "Player") -> String -> IO ()
look handle player "" = lookRoom handle player
look handle player "me" = lookSelf handle player
look handle player targetName = do
  response <- atomically $
    runEdgy $ do
      room <- getRelated @"location" player
      people <-
        filterM
          ( \person -> do
              name <- getAttribute @"name" person
              return
                ( any
                    (== map toLower targetName)
                    (map toLower name : words (map toLower name))
                )
          )
          =<< getRelated @"population" room
      case people of
        person : _ -> return (lookPlayer handle person)
        _ -> do
          items <-
            (++)
              <$> getRelated @"contents" room
              <*> getRelated @"inventory" player
          matchingItems <- filterM (goesBy targetName) items
          case matchingItems of
            item : _ -> return (lookItem handle item)
            _ -> do
              exits <- filterM (goesBy targetName) =<< getRelated @"exit" room
              case exits of
                exit : _ -> return (lookExit handle exit)
                _ -> return $ do
                  hPutStrLn handle $
                    "Could not find anything called " ++ targetName ++ "."
                  return ()
  response

lookRoom :: Handle -> Node MUDSchema (DataNode "Player") -> IO ()
lookRoom handle player = do
  output <- atomically $
    runEdgy $ do
      room <- getRelated @"location" player
      exits <- getRelated @"exit" room
      people <- filter (/= player) <$> getRelated @"population" room
      items <- getRelated @"contents" room
      unlines
        <$> sequence
          [ (\n -> "You are in " ++ n ++ ".") <$> getAttribute @"name" room,
            getAttribute @"description" room,
            unlines
              <$> sequence
                [ getAttribute @"direction" exit >>= \case
                    Just dir ->
                      (\n -> "You can go " ++ map toLower (show dir) ++ " to " ++ n ++ ".")
                        <$> getAttribute @"name" exit
                    Nothing -> (\n -> "You can go to " ++ n ++ ".") <$> getAttribute @"name" exit
                  | exit <- exits
                ],
            unlines
              <$> sequence
                [ (\n -> n ++ " is here.") <$> getAttribute @"name" p
                  | p <- people
                ],
            unlines
              <$> sequence
                [ (\n -> "There is a " ++ n ++ " here.") <$> getAttribute @"name" i
                  | i <- items
                ]
          ]
  hPutStrLn handle output

lookSelf :: Handle -> Node MUDSchema (DataNode "Player") -> IO ()
lookSelf handle player = do
  output <- atomically $
    runEdgy $ do
      items <- getRelated @"inventory" player
      unlines
        <$> sequence
          [ (\n -> "You are " ++ n ++ ".") <$> getAttribute @"name" player,
            getAttribute @"description" player,
            unlines
              <$> sequence
                [ (\n -> "You are carrying a " ++ n ++ ".") <$> getAttribute @"name" i
                  | i <- items
                ]
          ]
  hPutStrLn handle output

lookPlayer :: Handle -> Node MUDSchema (DataNode "Player") -> IO ()
lookPlayer handle player = do
  output <- atomically $
    runEdgy $ do
      name <- getAttribute @"name" player
      items <- getRelated @"inventory" player
      unlines
        <$> sequence
          [ pure ("You see " ++ name ++ "."),
            getAttribute @"description" player,
            unlines
              <$> sequence
                [ (\n -> name ++ " is carrying a " ++ n ++ ".") <$> getAttribute @"name" i
                  | i <- items
                ]
          ]
  hPutStrLn handle output

lookItem :: Handle -> Node MUDSchema (DataNode "Item") -> IO ()
lookItem handle item = do
  output <- atomically $
    runEdgy $ do
      unlines
        <$> sequence
          [ (\n -> "You see " ++ n ++ ".") <$> getAttribute @"name" item,
            getAttribute @"description" item
          ]
  hPutStrLn handle output

lookExit :: Handle -> Node MUDSchema (DataNode "Exit") -> IO ()
lookExit handle exit = do
  output <- atomically $
    runEdgy $ do
      name <- getAttribute @"name" exit
      summary <-
        getAttribute @"direction" exit >>= \case
          Just direction ->
            return $
              "You see a "
                ++ name
                ++ " to the "
                ++ map toLower (show direction)
                ++ "."
          Nothing -> return $ "You see a " ++ name ++ "."
      desc <- getAttribute @"description" exit
      return (unlines [summary, desc])
  hPutStrLn handle output
  return ()

go :: Handle -> Node MUDSchema (DataNode "Player") -> Either Direction String -> IO ()
go handle player target = do
  success <- atomicallySync $
    runEdgy $ do
      exits <-
        filterM (matchesTarget target)
          =<< getRelated @"exit"
          =<< getRelated @"location" player
      case exits of
        [] -> return False
        (exit : _) -> do
          destination <- getRelated @"destination" exit
          setRelated @"location" player destination
          return True
  case success of
    True -> look handle player ""
    False -> hPutStrLn handle "You can't go that way!"
  where
    matchesTarget ::
      Either Direction String ->
      Node MUDSchema (DataNode "Exit") ->
      Edgy MUDSchema Bool
    matchesTarget (Left dir) exit = (== Just dir) <$> getAttribute @"direction" exit
    matchesTarget (Right way) exit = goesBy way exit

inventory :: Handle -> Node MUDSchema (DataNode "Player") -> IO ()
inventory handle player = do
  items <-
    atomically $
      runEdgy $
        traverse (getAttribute @"name") =<< getRelated @"inventory" player
  when (null items) $ hPutStrLn handle $ "You are not carrying anything."
  forM_ items $ \i -> hPutStrLn handle $ i ++ " is in your inventory."

takeItem :: Handle -> Node MUDSchema (DataNode "Player") -> String -> IO ()
takeItem handle player itemName = do
  result <- atomically $
    runEdgy $ do
      items <-
        filterM (goesBy itemName)
          =<< getRelated @"contents"
          =<< getRelated @"location" player
      case items of
        [] -> return Nothing
        (item : _) -> do
          addRelated @"inventory" player item
          clearRelated @"location" item
          Just <$> getAttribute @"name" item
  case result of
    Nothing -> hPutStrLn handle "You can't find that item!"
    Just item -> hPutStrLn handle $ "You pick up " ++ item ++ "."

dropItem :: Handle -> Node MUDSchema (DataNode "Player") -> String -> IO ()
dropItem handle player itemName = do
  result <- atomically $
    runEdgy $ do
      items <- filterM (goesBy itemName) =<< getRelated @"inventory" player
      case items of
        [] -> return Nothing
        (item : _) -> do
          here <- getRelated @"location" player
          removeRelated @"inventory" player item
          addRelated @"contents" here item
          Just <$> getAttribute @"name" item
  case result of
    Nothing -> hPutStrLn handle "You aren't carrying that item!"
    Just item -> hPutStrLn handle $ "You drop " ++ item ++ "."

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
