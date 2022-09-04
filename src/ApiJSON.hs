{-# LANGUAGE DeriveGeneric #-}

module ApiJSON (
  Submission (..),
  parseSubmissions,
  loadSubmissions,
  ) where

import GHC.Generics (Generic)
import Data.List (stripPrefix)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import Data.Aeson (FromJSON, genericParseJSON)
import qualified Data.Aeson as JSON


newtype Submissions =
  Submissions
  { submissions :: [Submission] }
  deriving (Show, Generic)

instance FromJSON Submissions
-- instance ToJSON Submissions where

data Submission =
  Submission
  { sub_id :: Int
  , sub_problem_id :: Int
  , sub_submitted_at :: String
  , sub_status :: String
  , sub_score :: Int
  , sub_error :: String
  }
  deriving (Show, Generic)

instance FromJSON Submission where
  parseJSON = genericParseJSON $ stripPrefixOptions "sub_"

-- instance ToJSON Submission where
--   toJSON = genericToJSON $ stripPrefixOptions "sub_"

-- | parse output of api/list-submissions.sh
parseSubmissions :: ByteString -> Either String [Submission]
parseSubmissions = (submissions <$>) . JSON.eitherDecodeStrict'

-- | load from file which content is output of api/list-submissions.sh
loadSubmissions :: FilePath -> IO (Either String [Submission])
loadSubmissions = (parseSubmissions <$>) . BS.readFile

---

stripPrefix' :: Eq a => [a] -> [a] -> [a]
stripPrefix' prefix s = maybe s id $ stripPrefix prefix s

stripPrefixOptions :: String -> JSON.Options
stripPrefixOptions prefix = JSON.defaultOptions { JSON.fieldLabelModifier = stripPrefix' prefix }
