module Models where

import Data.Text (Text)
import Data.Time (Day, UTCTime)

data ResourceType = ResourceType
  { rtId :: Int
  , rtName :: Text
  } deriving (Show, Eq)

data Resource = Resource
  { rId :: Int
  , rName :: Text
  , rTypeId :: Int
  , rContent :: Maybe Text
  , rPurpose :: Maybe Text
  , rOpenedAt :: Maybe Day
  } deriving (Show, Eq)

data Author = Author
  { aId :: Int
  , aFullName :: Text
  , aDept :: Maybe Text
  } deriving (Show, Eq)

data User = User
  { uId :: Int
  , uUsername :: Text
  , uEmail :: Maybe Text
  , uRole :: Text
  } deriving (Show, Eq)

data UsageEvent = UsageEvent
  { ueId  :: Integer
  , ueResourceId :: Int
  , ueUserId :: Maybe Int
  , ueEventAt :: UTCTime
  , ueAction :: Text
  } deriving (Show, Eq)
