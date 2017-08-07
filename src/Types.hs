{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, FlexibleInstances #-}
module Types (
  Rate.Rate,
  TopList.TopList,
  module Web.Twitter.Types,
  module Types
) where

import Rate
import TopList

import Data.Data
import Data.Aeson

import Numeric (readHex)
import Data.Char (chr)

import Control.Concurrent.MVar
import Control.Monad (mzero)

import qualified Data.Text as T

import Web.Twitter.Types (HashTagEntity, URLEntity, MediaEntity)

data Command = Die | Report (MVar Stats)

instance Show Command where
  show Die = "Die"
  show (Report _) = "Report"

data ReportResult = ReportResult Stats

data StatResult = StatResult {
  srHasUrl :: Bool,
  srHasPhotoUrl :: Bool,
  srDomains :: [Domain],
  srHashTags :: [HashTag],
  srEmojies :: [Emoji]
}

data TweetInfo = TweetInfo {
  tText :: T.Text,
  tHashTags :: [HashTagEntity],
  tUrls :: [URLEntity],
  tMediaUrls :: [MediaEntity]
}

newtype Emoji = Emoji {
  unEmoji :: T.Text
} deriving (Eq, Ord, Show)

newtype Domain = Domain {
  unDomain :: T.Text
} deriving (Eq, Ord, Show)
newtype HashTag = HashTag {
  unHashTag :: T.Text
} deriving (Eq, Ord, Show)

data Stats = Stats {
  recentTweets :: !Integer, -- | for rate calculations.
  totalTweets :: !Integer,

  emojiTweets :: !Integer, -- | number of tweets with any emoji
  urlTweets :: !Integer, -- | number of tweets with any url at all.
  photoUrlTweets :: !Integer, -- | number of tweets with any embedded media (not just photo)

  tweetRate :: !Rate, -- | tweets per hour / minute / second

  topHashTags :: !(TopList HashTag),
  topEmojies :: !(TopList Emoji),
  topDomains :: !(TopList Domain)
} deriving Show

data EmojiData a = EmojiData {
  edName :: a,
  edCodePoints :: a
--   edHasTwitter :: Bool -- I don't think there is a need for this.
} deriving (Show, Data, Functor)

-- name is sometimes null
-- short_name never null, apparently always unique
instance FromJSON (EmojiData String) where
  parseJSON (Object o) =  EmojiData <$> o .: "short_name" <*> (toCodePoint <$> o .: "unified") -- <*> o .: "has_img_twitter"
  parseJSON _ = mzero

toCodePoint :: T.Text -> String
toCodePoint t = concat $ map (foo . T.unpack) (T.splitOn "-" t)
   where
     foo :: String -> String
     foo str = case readHex str of
       [(c,_)] -> [(chr c)]
       _ -> error "unable to create codepoint"
