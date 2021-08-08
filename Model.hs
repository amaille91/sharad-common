{-# LANGUAGE DeriveGeneric         #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE AllowAmbiguousTypes   #-}
-- {-# LANGUAGE PolyKinds             #-}
-- {-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleContexts      #-}

module Model (Note(..), NoteContent(..), NoteUpdate(..), StorageId(..), Checklist(..), ChecklistContent(..), ChecklistUpdate(..), ChecklistItem(..), Content, Update, Identifiable, storageId, GettableContent, getContent, ContentType) where


import           Data.Aeson
import           GHC.Generics (Generic)

class Identifiable a where
  storageId :: a -> StorageId

class (Eq a, Show a, ToJSON a) => Content a where
 
class (Show a, ToJSON a, Content (UpdateContentType a)) => Update a where
  type UpdateContentType a :: * 
  targetId :: a -> StorageId
  newContent :: a -> UpdateContentType a

class (FromJSON a, Content (ContentType a)) => GettableContent a where
  type ContentType a :: *
  getContent ::  a -> ContentType a

data NoteContent = NoteContent { title   :: Maybe String
                               , content :: String
                               } deriving(Show, Generic, Eq)

instance Content NoteContent where
instance Content ChecklistContent where

instance FromJSON NoteContent
instance ToJSON NoteContent

data StorageId = StorageId { id      :: String
                           , version :: String
                           } deriving (Show, Generic, Eq)

instance ToJSON StorageId
instance FromJSON StorageId

data Note = Note { noteId :: StorageId
                 , noteContent :: NoteContent
                 } deriving (Show, Generic, Eq)

instance ToJSON Note
instance FromJSON Note
instance Identifiable Note where
  storageId = noteId
instance GettableContent Note where
  type ContentType Note = NoteContent
  getContent = noteContent


data NoteUpdate = NoteUpdate StorageId NoteContent deriving (Show, Generic)

instance FromJSON NoteUpdate
instance ToJSON NoteUpdate
instance Update NoteUpdate where
  type UpdateContentType NoteUpdate = NoteContent
  targetId (NoteUpdate _id _) = _id
  newContent (NoteUpdate _ content) = content

-- ======================= CHECKLIST =======================================

data ChecklistContent = ChecklistContent { name :: String
                                         , items :: [ChecklistItem]
                                         } deriving (Show, Generic, Eq)

instance FromJSON ChecklistContent
instance ToJSON ChecklistContent

data ChecklistItem = ChecklistItem { label :: String
                                   , checked :: Bool
                                   } deriving (Show, Generic, Eq)

instance FromJSON ChecklistItem
instance ToJSON ChecklistItem

data Checklist = Checklist { checklistId :: StorageId
                           , checklistContent :: ChecklistContent
                           } deriving (Show, Generic, Eq)

instance ToJSON Checklist
instance FromJSON Checklist
instance Identifiable Checklist where
  storageId = checklistId
instance GettableContent Checklist where
  type ContentType Checklist = ChecklistContent
  getContent = checklistContent


data ChecklistUpdate = ChecklistUpdate StorageId ChecklistContent deriving (Show, Generic, Eq)

instance FromJSON ChecklistUpdate
instance ToJSON ChecklistUpdate
instance Update ChecklistUpdate where
  type UpdateContentType ChecklistUpdate = ChecklistContent
  targetId (ChecklistUpdate _id _) = _id
  newContent (ChecklistUpdate _ content) = content
