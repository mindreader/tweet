{-# LANGUAGE OverloadedStrings, TemplateHaskell, FlexibleInstances #-}
module EmojiData (
  twitterEmojiDataQ,
--  twitterEmojiData,
) where

import Types

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import qualified Data.ByteString.Lazy as B
import Data.Aeson

-- Workaround for template haskell not being able to liftData T.Text,
-- instead stores emojiData as String, then fmaps it to Text after it is loaded.
twitterEmojiData :: IO [EmojiData String]
twitterEmojiData = do
  decode <$> B.readFile "emoji.json" >>= \case
    Just v -> return $ (v :: [EmojiData String])
    Nothing -> error "unable to read emoji.json"

twitterEmojiDataQ :: Q Exp
twitterEmojiDataQ = runIO twitterEmojiData >>= liftData
