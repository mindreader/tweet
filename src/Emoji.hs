{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Emoji (scrapeEmojies, emojiData) where

import Types

import qualified Data.Text as T

import qualified Data.Text.Internal.Search as T (indices)
import EmojiData

scrapeEmojies :: T.Text -> [EmojiData T.Text] -> [Emoji]
scrapeEmojies tweet = concatMap find
  where
    find :: EmojiData T.Text -> [Emoji]
    find em =
       let count = (length (T.indices (edCodePoints em) tweet))
       in replicate count (Emoji $ edName em)

emojiData :: [EmojiData T.Text]
emojiData = fmap T.pack <$> $(twitterEmojiDataQ)
