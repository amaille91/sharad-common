{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Model (
  NoteContent(..), StorageId(..)
  , ChecklistContent(..), ChecklistItem(..)
  , Content
  , Identifiable(..), hash
  ) where


import           Data.Aeson
import qualified Data.ByteString.Base64.Lazy as Base64 (encode)
import           Data.ByteString.Lazy.Char8  as BL
import           Data.Digest.Pure.SHA        (bytestringDigest, sha256)
import           GHC.Generics                (Generic)

data StorageId = StorageId { id      :: String
                           , version :: String
                           } deriving (Show, Generic, Eq)

instance ToJSON   StorageId
instance FromJSON StorageId

class (Show a, Eq a, ToJSON a, FromJSON a) => Content a where
  hash :: a -> String

data Content a => Identifiable a =
    Identifiable { storageId :: StorageId, content :: a } deriving (Eq, Show)

instance Content a => ToJSON (Identifiable a) where
  toJSON (Identifiable {..}) =
    object [ "storageId" .= storageId, "content" .= content ]

instance Content a => FromJSON (Identifiable a) where
  parseJSON = withObject "Identifiable" $ \value -> Identifiable
    <$> value .: "storageId"
    <*> value .: "content"

-- ===================== Note =============================================

data NoteContent = NoteContent { title       :: Maybe String
                               , noteContent :: String
                               } deriving (Show, Generic, Eq)

instance Content NoteContent where
  hash content = base64Sha256 $ show content

instance FromJSON NoteContent
instance ToJSON   NoteContent

-- ======================= CHECKLIST =======================================

data ChecklistContent = ChecklistContent { name  :: String
                                         , items :: [ChecklistItem]
                                         } deriving (Show, Generic, Eq)

data ChecklistItem    = ChecklistItem    { label   :: String
                                         , checked :: Bool
                                         } deriving (Show, Generic, Eq)

instance FromJSON ChecklistContent
instance ToJSON   ChecklistContent

instance FromJSON ChecklistItem
instance ToJSON   ChecklistItem

instance Content ChecklistContent where
  hash checklistContent = base64Sha256 $ show checklistContent

-- =============================== Utils ==========================================

base64Sha256 :: String -> String
base64Sha256 contentToHash = BL.unpack . Base64.encode . bytestringDigest . sha256 $ BL.pack contentToHash
