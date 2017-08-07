{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Rate (newRate, smhHits, moreHits, Rate) where

data Rate = Rate {
  rLatestSec :: !Integer, -- | this many hits in the last second
  rSecs :: ![Integer],    -- | hits in 59 seconds not counting latest sec
  rMins :: ![Integer],    -- | hits in 59 minutes not counting latest min
  rSecCount :: !Integer   -- | number of seconds that have gone by (limit 3600)
} deriving Show


newRate :: Rate
newRate = Rate 0 [] [] 0

moreHits :: Integer -> Rate -> Rate
moreHits hits (Rate _ s m 0) = Rate hits s m 1
moreHits hits (Rate ls s m secs) =
  let news = ls : take 58 s
      newm = if secs `mod` 60 == 0
        then ls + sum s : take 58 m
        else m

  in Rate hits news newm (min 3600 (secs + 1))

-- | second, minute, hourly hit rate rounded to nearest ints.
smhHits :: Rate -> (Integer, Integer, Integer)

-- most common case, been running for over an hour
smhHits (Rate latsec secs mins 3600) =
  let minsum = latsec + sum secs
  in (latsec, minsum, minsum + sum mins)

-- if not, extrapolate
smhHits (Rate _ _ _ 0) = (0, 0, 0)
smhHits (Rate latsec secs mins secCount) =
  let
    minfrac = if secCount < 60 then 60 / (toRational secCount) else 1
    hourfrac = 60 / (toRational ((secCount - 1) `div` 60 + 1))

    minavg = (toRational $ latsec + sum secs) * minfrac
    houravg = (minavg + toRational (sum mins)) * hourfrac
  in (latsec, round minavg, round houravg)
