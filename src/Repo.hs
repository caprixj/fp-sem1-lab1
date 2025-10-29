{-# LANGUAGE OverloadedStrings #-}
module Repo
  (
    fetchAllResources
  , getResource
  , insertResource
  , updateResourceTitle
  , deleteResource
  , searchResources
  , listUsers
  , insertUser
  , listAuthors
  , insertAuthor
  , deleteAuthor
  , assignAuthorToResource
  , getAuthorsForResource
  , updateResourceMeta
  , insertUsageEvent
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple ( Connection, query, query_, execute, changes )
import Database.SQLite.Simple.Types (Only (..))
import Data.Int (Int64)
import Data.Time (Day)

import Models


fetchLastInsertId :: Connection -> IO Int
fetchLastInsertId conn = do
  rows <- query_ conn "SELECT last_insert_rowid()" :: IO [Only Int64]
  case rows of
    Only i : _ -> pure (fromIntegral i)
    _ -> pure 0

parseResourceRow
  :: (Int, Text, Int, Maybe Text, Maybe Text, Maybe Day)
  -> Resource
parseResourceRow (i, n, ty, co, pu, openedAt) =
  Resource i n ty co pu openedAt

parseUserRow :: (Int, Text, Maybe Text, Text) -> User
parseUserRow (i, u, e, r) = User i u e r

fetchAllResources :: Connection -> IO [Resource]
fetchAllResources conn = do
  rows <- query_ conn
    "SELECT id, name, type_id, content, purpose, opened_at \
    \FROM resources ORDER BY id DESC"
    :: IO [(Int, Text, Int, Maybe Text, Maybe Text, Maybe Day)]
  pure (map parseResourceRow rows)

getResource :: Connection -> Int -> IO (Maybe Resource)
getResource conn rid = do
  rows <- query conn
    "SELECT id, name, type_id, content, purpose, opened_at \
    \FROM resources WHERE id = ?"
    (Only rid)
    :: IO [(Int, Text, Int, Maybe Text, Maybe Text, Maybe Day)]
  pure $ case rows of
    []  -> Nothing
    x:_ -> Just (parseResourceRow x)

insertResource
  :: Connection
  -> Text -> Int -> Maybe Text -> Maybe Text -> Maybe Text
  -> IO Int
insertResource conn name typeId content purpose openedAt = do
  let q = "INSERT INTO resources (name, type_id, content, purpose, opened_at) \
          \VALUES (?,?,?,?,?)"
  _ <- execute conn q (name, typeId, content, purpose, openedAt)
  fetchLastInsertId conn

updateResourceTitle :: Connection -> Int -> Text -> IO Int
updateResourceTitle conn rid newTitle = do
  execute conn "UPDATE resources SET name=? WHERE id=?" (newTitle, rid)
  changes conn

deleteResource :: Connection -> Int -> IO Int
deleteResource conn rid = do
  execute conn "DELETE FROM resources WHERE id=?" (Only rid)
  changes conn

searchResources :: Connection -> Text -> IO [Resource]
searchResources conn needle = do
  let like = T.concat ["%", needle, "%"]
  rows <- query conn
    "SELECT id, name, type_id, content, purpose, opened_at \
    \FROM resources WHERE name LIKE ? OR content LIKE ? OR purpose LIKE ? ORDER BY id DESC"
    (like, like, like)
    :: IO [(Int, Text, Int, Maybe Text, Maybe Text, Maybe Day)]
  pure (map parseResourceRow rows)


listUsers :: Connection -> IO [User]
listUsers conn = do
  rows <- query_ conn "SELECT id, username, email, role FROM users ORDER BY id DESC"
    :: IO [(Int, Text, Maybe Text, Text)]
  pure (map parseUserRow rows)

insertUser :: Connection -> Text -> Maybe Text -> Text -> IO Int
insertUser conn username email role = do
  _ <- execute conn "INSERT INTO users (username, email, role) VALUES (?,?,?)"
        (username, email, role)
  fetchLastInsertId conn


listAuthors :: Connection -> IO [Author]
listAuthors conn = do
  rows <- query_ conn "SELECT id, full_name, dept FROM authors ORDER BY id DESC"
    :: IO [(Int, Text, Maybe Text)]
  pure (map (\(i,n,d) -> Author i n d) rows)

insertAuthor :: Connection -> Text -> Maybe Text -> IO Int
insertAuthor conn fullName dept = do
  _ <- execute conn "INSERT INTO authors (full_name, dept) VALUES (?,?)" (fullName, dept)
  fetchLastInsertId conn

deleteAuthor :: Connection -> Int -> IO Int
deleteAuthor conn aid = do
  execute conn "DELETE FROM authors WHERE id=?" (Only aid)
  changes conn

assignAuthorToResource :: Connection -> Int -> Int -> IO Int
assignAuthorToResource conn rid aid = do
  execute conn "INSERT OR IGNORE INTO resource_authors (resource_id, author_id) VALUES (?,?)" (rid, aid)
  changes conn

getAuthorsForResource :: Connection -> Int -> IO [Author]
getAuthorsForResource conn rid = do
  rows <- query conn
    "SELECT a.id, a.full_name, a.dept \
    \FROM resource_authors ra JOIN authors a ON a.id = ra.author_id \
    \WHERE ra.resource_id = ? ORDER BY a.full_name"
    (Only rid)
    :: IO [(Int, Text, Maybe Text)]
  pure (map (\(i,n,d) -> Author i n d) rows)


updateResourceMeta
  :: Connection
  -> Int
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Int
  -> IO Int
updateResourceMeta conn rid mName mContent mPurpose mOpenedAt mTypeId = do
  let q =
        "UPDATE resources SET \
        \  name       = COALESCE(?, name), \
        \  content    = COALESCE(?, content), \
        \  purpose    = COALESCE(?, purpose), \
        \  opened_at  = COALESCE(?, opened_at), \
        \  type_id    = COALESCE(?, type_id) \
        \WHERE id = ?"
  execute conn q (mName, mContent, mPurpose, mOpenedAt, mTypeId, rid)
  changes conn


insertUsageEvent :: Connection -> Int -> Maybe Int -> Text -> IO Int
insertUsageEvent conn rid mUid action = do
  execute conn "INSERT INTO usage_events (resource_id, user_id, action) VALUES (?,?,?)" (rid, mUid, action)
  changes conn
  