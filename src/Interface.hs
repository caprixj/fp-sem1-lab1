{-# LANGUAGE OverloadedStrings #-}
module Interface (runInterface) where

import Database
import Repo
import Models

import Database.SQLite.Simple (Connection)

import qualified Data.Text as T
import Data.Text (Text)
import System.IO (hFlush, stdout, hSetBuffering, BufferMode(LineBuffering))
import Text.Read (readMaybe)

runInterface :: IO ()
runInterface = do
  hSetBuffering stdout LineBuffering
  withPool $ \pool -> withConn pool $ \conn -> mainLoop conn


mainLoop :: Connection -> IO ()
mainLoop conn = do
  putStrLn ""
  putStrLn "╔═══════════════════════════════════╗"
  putStrLn "║       Faculty Digital Archive     ║"
  putStrLn "╚═══════════════════════════════════╝"
  putStrLn "┌─ Resources ───────────────────────┐"
  putStrLn "│ 1) List Resources                 │"
  putStrLn "│ 2) View Resource Details          │"
  putStrLn "│ 3) Add Resource                   │"
  putStrLn "│ 4) Search Resources               │"
  putStrLn "│ 5) Delete Resource                │"
  putStrLn "│ 6) Update Resource                │"
  putStrLn "├─ Authors ─────────────────────────┤"
  putStrLn "│ 7) List Authors                   │"
  putStrLn "│ 8) Add Author                     │"
  putStrLn "│ 9) Delete Author                  │"
  putStrLn "│10) Link Author to Resource        │"
  putStrLn "│11) Show Authors for Resource      │"
  putStrLn "├─ Users & Usage ───────────────────┤"
  putStrLn "│12) List Users                     │"
  putStrLn "│13) Add User                       │"
  putStrLn "│14) Log Usage Event                │"
  putStrLn "├─ System ──────────────────────────┤"
  putStrLn "│ 0) Exit                           │"
  putStrLn "└───────────────────────────────────┘"
  choice <- promptInt "> Your choice (0-14):"

  case choice of
    1 -> do
      rs <- fetchAllResources conn
      mapM_ (putStrLn . T.unpack . formatResourceLine) rs
      pressEnterToContinue >> mainLoop conn
    2 -> do
      rid <- promptInt "> Resource ID to view:"
      mRes <- getResource conn rid
      case mRes of
        Nothing -> putStrLn "Resource not found."
        Just res -> print res
      pressEnterToContinue >> mainLoop conn
    3 -> do
      t  <- promptText "> Resource name:"
      ty <- promptInt  "> type_id (see resource_types):"
      rid <- insertResource conn t ty Nothing Nothing Nothing
      putStrLn $ "Added resource id=" ++ show rid
      pressEnterToContinue >> mainLoop conn
    4 -> do
      q <- promptText "> Search term:"
      rs <- searchResources conn q
      mapM_ (putStrLn . T.unpack . formatResourceLine) rs
      pressEnterToContinue >> mainLoop conn
    5 -> do
      i <- promptInt "> Resource ID to delete:"
      n <- deleteResource conn i
      putStrLn $ "Rows deleted: " ++ show n
      pressEnterToContinue >> mainLoop conn
    6 -> do
      rid <- promptInt "> Resource ID to update:"
      putStrLn "New value or empty to keep unchanged."
      nt  <- promptMaybeText "> name:"
      nc  <- promptMaybeText "> content:"
      np  <- promptMaybeText "> purpose:"
      no  <- promptMaybeText "> opened_at (YYYY-MM-DD):"
      nty <- promptMaybeInt  "> type_id:"
      changed <- updateResourceMeta conn rid nt nc np no nty
      putStrLn $ "Columns updated: " ++ show changed
      pressEnterToContinue >> mainLoop conn

    7 -> do
      as <- listAuthors conn
      mapM_ print as
      pressEnterToContinue >> mainLoop conn
    8 -> do
      n  <- promptText "> full_name:"
      dp <- promptMaybeText "> dept (empty = null):"
      aid <- insertAuthor conn n dp
      putStrLn $ "Created author id=" ++ show aid
      pressEnterToContinue >> mainLoop conn
    9 -> do
      aid <- promptInt "> Author ID to delete:"
      n   <- deleteAuthor conn aid
      putStrLn $ "Rows deleted: " ++ show n
      pressEnterToContinue >> mainLoop conn
    10 -> do
      rid <- promptInt "> Resource ID:"
      aid <- promptInt "> Author ID:"
      n   <- assignAuthorToResource conn rid aid
      putStrLn $ "Link added (rows inserted): " ++ show n
      pressEnterToContinue >> mainLoop conn
    11 -> do
      rid <- promptInt "> Resource ID:"
      as  <- getAuthorsForResource conn rid
      if null as then putStrLn "No authors found." else mapM_ print as
      pressEnterToContinue >> mainLoop conn

    12 -> do
      us <- listUsers conn
      mapM_ print us
      pressEnterToContinue >> mainLoop conn
    13 -> do
      u  <- promptText "> username:"
      em <- promptMaybeText "> email (empty = null):"
      ro <- promptText "> role (student/staff/admin):"
      uid <- insertUser conn u em ro
      putStrLn $ "Created user id=" ++ show uid
      pressEnterToContinue >> mainLoop conn

    14 -> do
      rid <- promptInt "> Resource ID:"
      uid <- promptMaybeInt "> user_id (empty = NULL):"
      act <- promptAction
      n   <- insertUsageEvent conn rid uid act
      putStrLn $ "Events logged: " ++ show n
      pressEnterToContinue >> mainLoop conn

    0 -> putStrLn "Bye!"
    _ -> do
      putStrLn "Invalid choice."
      pressEnterToContinue >> mainLoop conn


promptText :: String -> IO Text
promptText msg = do
  putStrPrompt msg
  T.pack <$> getLine

promptMaybeText :: String -> IO (Maybe Text)
promptMaybeText msg = do
  putStrPrompt msg
  s <- getLine
  pure $ if null s then Nothing else Just (T.pack s)

promptInt :: String -> IO Int
promptInt msg = do
  putStrPrompt msg
  line <- getLine
  case readMaybe line :: Maybe Int of
    Just n  -> pure n
    Nothing -> putStrLn "Must be an integer." >> promptInt msg

promptMaybeInt :: String -> IO (Maybe Int)
promptMaybeInt msg = do
  putStrPrompt msg
  line <- getLine
  if null line then pure Nothing
  else case readMaybe line :: Maybe Int of
         Just n  -> pure (Just n)
         Nothing -> putStrLn "Must be an integer or empty." >> promptMaybeInt msg

promptAction :: IO Text
promptAction = do
  putStrLn "Select action: 1=view, 2=download, 3=like, 4=bookmark"
  n <- promptInt "> #:"
  case n of
    1 -> pure "view"
    2 -> pure "download"
    3 -> pure "like"
    4 -> pure "bookmark"
    _ -> putStrLn "Invalid choice." >> promptAction

pressEnterToContinue :: IO ()
pressEnterToContinue = do
  putStrPrompt "Press Enter to continue..."
  _ <- getLine
  pure ()

putStrPrompt :: String -> IO ()
putStrPrompt msg = do
  putStr (msg <> " ")
  hFlush stdout

formatResourceLine :: Resource -> Text
formatResourceLine r = T.intercalate " | "
  [ "ID: " <> T.pack (show (rId r))
  , rName r
  , "Type: " <> T.pack (show (rTypeId r))
  ]
