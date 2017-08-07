module Command(reportLoop, commandControl)  where

import Types
import Rate as R
import TopList as TL

import Data.Function
import Text.Printf

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (forever)

import Data.Monoid

import qualified Data.Text as T

reportLoop :: TChan Command -> IO ()
reportLoop comchan = forever $ do
  getReport comchan >>= putStrLn . renderReport Short
  threadDelay 5000000
 

-- For debugging.
commandControl :: TChan Command -> IO ()
commandControl chan = do
    putStrLn "Type help."
    fix $ \loop -> getLine >>= \case
      "help" -> putStrLn "Available commands: help, die, q, state, report" >> loop
      str | str == "q" || str == "die" -> atomically (writeTChan chan Die)
      "state" -> getReport chan >>= putStrLn . show >> loop
      "report" -> getReport chan >>= putStrLn . renderReport Long >> loop
      c -> putStrLn ("unrecognized command: " ++ show c) >> loop
 

getReport :: TChan Command -> IO Stats
getReport chan = do
  m <- newEmptyMVar
  atomically $ writeTChan chan $ Report m
  readMVar m

data ReportLength = Long | Short

renderReport :: ReportLength -> Stats -> String
renderReport rl s = case rl of
  Long -> 
    "Total Tweets: " <> show (totalTweets s) <> "\n" <> 
    "Average Tweets per second:" <> show tps <> " minute:" <> show tpm  <> " hour:" <> show tph <> "\n" <>
    "Top 5 emojis: " <> (T.unpack $ T.intercalate ", " topemos) <> "\n" <>
    "Percent of tweets with emojis: " <> emoPerc <> "\n" <>
    "Top 5 hashtags: " <> (T.unpack $ T.intercalate ", " tophashtags) <> "\n" <>
    "Percent of tweets with urls: " <> urlPerc <> "\n" <>
    "Percent of tweets with photo: " <> photoPerc <> "\n" <>
    "Top 5 domains: " <> (T.unpack $ T.intercalate ", " topdomains)

  Short ->
    "Total tweets so far: " <> show (totalTweets s) <> ", tweets per sec/min/hour: " <> show tps <> "/" <> show tpm <> "/" <> show tph <> "\n" <>
    "Percentage with emoijs: " <> emoPerc <> ", urls: " <> urlPerc <>  ", photo urls: " <> photoPerc <> "\n" <>
    "Top 5:\n" <>
    "  hashtags: " <> (T.unpack $ T.intercalate ", " tophashtags) <> "\n" <>
    "  emojis: " <> (T.unpack $ T.intercalate ", " topemos) <> "\n" <>
    "  domains: " <> (T.unpack $ T.intercalate ", " topdomains)<> "\n"
  where
    (tps, tpm, tph) = s & tweetRate & R.smhHits

    perc :: (Stats -> Integer) -> Double
    perc f = if totalTweets s == 0 then 0
      else 100 * (realToFrac $ toRational (f s) / toRational (totalTweets s))

    topemos = s & topEmojies & TL.top 5 & map (unEmoji . fst)
    tophashtags = s & topHashTags & TL.top 5 & map (("#" <>) . unHashTag . fst)
    topdomains = s & topDomains & TL.top 5 & map (unDomain . fst)

    emoPerc = printf "%.2f%%" $ perc emojiTweets
    urlPerc = printf "%.2f%%" $ perc urlTweets 
    photoPerc = printf "%.2f%%" $ perc photoUrlTweets
