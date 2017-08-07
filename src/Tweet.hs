module Tweet (tweetInfo) where

import Types
import Emoji

import Data.Function
import Control.Applicative

import qualified Data.Text as T
import Control.Concurrent.STM

import Web.Twitter.Types (HashTagEntity(..), URLEntity(..))

import Network.URI

data Res = Tweet TweetInfo | Com Command

tweetInfo :: TChan Command -> TBQueue TweetInfo -> TBQueue StatResult -> IO ()
tweetInfo com twq resq = do
  let em = emojiData -- TODO What do to ensure this is shared?
  let getRes = Tweet <$> readTBQueue twq <|> Com <$> readTChan com
      loop = do
        atomically getRes >>= \case
          Com Die -> return ()

          Tweet tw -> do
            atomically $ writeTBQueue resq (getInfo em tw)
            loop

          _ -> loop

  loop

getInfo :: [EmojiData T.Text] -> TweetInfo -> StatResult
getInfo ed (TweetInfo txt hts urls meds) =
  StatResult hasUrl hasPhotoUrl doms hashTags emos

  where
    hasUrl = length urls > 0
    hasPhotoUrl = length meds > 0
    hashTags = HashTag . hashTagText <$> hts
    doms = concatMap urlsToDomains urls
    emos = scrapeEmojies txt ed

urlsToDomains :: URLEntity -> [Domain]
urlsToDomains ent = case ent & ueExpanded & T.unpack & parseAbsoluteURI of
  Just uri -> case uri & uriAuthority of
    Just uriauth -> [uriauth & uriRegName & T.pack & Domain]
    Nothing -> []
  Nothing -> []
