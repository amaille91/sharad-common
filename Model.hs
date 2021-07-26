{-# LANGUAGE DeriveGeneric #-}

module Model (Note(..), NoteContent(..), NoteUpdate(..), StorageId(..)) where


import           Data.Aeson
import           GHC.Generics (Generic)

data NoteContent = NoteContent { title   :: Maybe String
                               , content :: String
                               } deriving(Show, Generic, Eq)

instance FromJSON NoteContent
instance ToJSON NoteContent

data StorageId = StorageId { id      :: String
                           , version :: String
                           } deriving (Show, Generic, Eq)

instance ToJSON StorageId
instance FromJSON StorageId

data Note = Note { storageId   :: StorageId
                 , noteContent :: NoteContent
                 } deriving (Show, Generic, Eq)

instance ToJSON Note
instance FromJSON Note

data NoteUpdate = NoteUpdate { targetId   :: StorageId
                             , newContent :: NoteContent
                             } deriving (Show, Generic)

instance FromJSON NoteUpdate
instance ToJSON NoteUpdate
