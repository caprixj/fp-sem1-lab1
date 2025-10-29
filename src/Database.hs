{-# LANGUAGE OverloadedStrings #-}
module Database
  ( withDbConnection
  , DbPool
  , withPool
  , withConn
  , initializeDatabase
  ) where

import Control.Exception (bracket)
import Database.SQLite.Simple (Connection, open, close, execute_)
import Database.SQLite.Simple.Types (Query(..))
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Data.Text (Text)
import Data.Foldable ()

dbFile :: FilePath
dbFile = "archive.db"

schemaFile :: FilePath
schemaFile = "db/schema.sql"

dataFile :: FilePath
dataFile = "db/test_data.sql"

runScript :: Connection -> Text -> IO ()
runScript conn sqlScript = do
  let commands = T.splitOn ";" sqlScript
  let validCommands = filter (not . T.null . T.strip) commands
  mapM_ (execute_ conn . Query) validCommands

initializeDatabase :: IO ()
initializeDatabase = do
  putStrLn "Initializing database..."
  withDbConnection $ \conn -> do
    schemaSql <- TIO.readFile schemaFile
    runScript conn schemaSql
    putStrLn "Database schema checked."
    
    dataSql <- TIO.readFile dataFile
    runScript conn dataSql
    putStrLn "Test data checked."
  putStrLn "Database ready."

withDbConnection :: (Connection -> IO a) -> IO a
withDbConnection action = do
  conn <- open dbFile
  execute_ conn "PRAGMA foreign_keys = ON;"
  bracket (return conn) close action

type DbPool = Connection

withPool :: (DbPool -> IO a) -> IO a
withPool = withDbConnection

withConn :: DbPool -> (Connection -> IO a) -> IO a
withConn conn action = action conn
