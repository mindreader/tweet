{-# LANGUAGE ScopedTypeVariables #-}
module TopList (newTopList, TopList, top, moreHits) where

import qualified Data.Map.Strict as M
import Data.List (insertBy)
import Data.Function (on)

-- TODO toplist is case sensitive, perhaps should not be at least for hash tags.
newtype TopList a = TopList (M.Map a Integer) deriving Show

newTopList :: TopList a
newTopList = TopList M.empty

maxFromMap :: Ord v => Int -> M.Map k v -> [(k, v)]
maxFromMap num = take num . go [] [] . M.toList
  where
    go _ tops [] = tops
    go kvs tops ((k, v) : rest) =
      go ((k,v):kvs) (insertBy (flip compare `on` snd) (k,v) (take num tops)) rest

top :: Int -> TopList a  -> [(a, Integer)]
top n (TopList m) = maxFromMap n m

-- TODO ? Ord constraint could theoretically be removed.
moreHits :: forall a. Ord a => [a] -> TopList a -> TopList a
moreHits xs (TopList m) = TopList $ foldr go m xs
  where go x accum = M.alter inc x accum :: M.Map a Integer
        inc Nothing = Just $! 1
        inc (Just c) = Just $! (c + 1)
