module Stats where

import Types
import Rate as R
import TopList as TL

import Data.Function ((&))
import Control.Monad (forever, void)

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async

import Control.Applicative

data Res = Tick | Res StatResult | Com Command

defStats :: Stats
defStats = Stats {
  recentTweets = 0,
  totalTweets = 0,

  photoUrlTweets = 0,
  urlTweets = 0,
  emojiTweets = 0,

  tweetRate = newRate,

  topHashTags = newTopList,
  topEmojies = newTopList,
  topDomains = newTopList
}

statsProc :: TChan Command -> TBQueue StatResult -> IO ()
statsProc comchan resq = do
  ticks <- newTChanIO

  void $ async (tickProc ticks)

  let
    getRes = Tick <$ readTChan ticks <|>
             Com <$> readTChan comchan <|>
             Res <$> readTBQueue resq

    loop stats = atomically getRes >>= \case
      Tick -> loop (tick stats)
      Res st -> loop (updateStats stats st)
      Com Die -> return ()
      Com (Report mvar) -> do
        putMVar mvar stats
        loop stats

  loop defStats


tick :: Stats -> Stats
tick stats = stats {
    recentTweets = 0,
    tweetRate = R.moreHits (recentTweets stats) (tweetRate stats)
  }

updateStats :: Stats -> StatResult -> Stats
updateStats stats (StatResult url purl doms hts emos) = stats {
    recentTweets = recentTweets stats + 1,
    totalTweets = totalTweets stats + 1,

    urlTweets =
      let cur = urlTweets stats
      in if url then cur + 1 else cur,

    photoUrlTweets =
      let cur = photoUrlTweets stats
      in if purl then cur + 1 else cur,

    emojiTweets =
      let cur = emojiTweets stats
      in if length emos > 0 then cur + 1 else cur,

    topHashTags = topHashTags stats & TL.moreHits hts,
    topEmojies  = topEmojies stats & TL.moreHits emos,
    topDomains  = topDomains stats & TL.moreHits doms
  }

tickProc :: TChan () -> IO ()
tickProc tickchan = forever $ do
  -- TODO deal with eventual tick skew.
  threadDelay 1000000
  atomically $ writeTChan tickchan ()
