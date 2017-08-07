module Main where

import Twitter
import Tweet
import Stats
import Command

import Control.Concurrent.STM
import Control.Concurrent.Async

import Control.Concurrent

import Control.Monad

import qualified Data.ByteString.Char8 as B
import qualified Data.Text.IO as T
import Data.Ini.Config

import Web.Twitter.Conduit hiding (count)


twInfoFromConfig :: String -> String -> String -> String -> TWInfo
twInfoFromConfig oauthConsKey oauthConsSecret oauthToken oauthTokenSecret  =
    def { twToken = def { twOAuth = tokens, twCredential = credential}, twProxy = Nothing}
  where
    tokens :: OAuth
    tokens = twitterOAuth {
      oauthConsumerKey = B.pack oauthConsKey,
      oauthConsumerSecret = B.pack oauthConsSecret
    }

    credential :: Credential
    credential = Credential
      [("oauth_token", B.pack oauthToken), ("oauth_token_secret", B.pack oauthTokenSecret)]

twitterParser :: IniParser TWInfo
twitterParser = section "twitter" $ do
  twInfoFromConfig <$>
        fieldOf "oauth_consumer_key" string
    <*> fieldOf "oauth_consumer_secret" string
    <*> fieldOf "oauth_token" string
    <*> fieldOf "oauth_token_secret" string

main :: IO ()
main = do

  conf <- T.readFile "config.ini"
  let twInfo = case parseIniFile conf twitterParser of
                  Left e -> error e
                  Right c -> c

  caps <- getNumCapabilities

  (tweetQueue, resultQueue) <- (,) <$> newTBQueueIO 1000 <*> newTBQueueIO (caps * 1000)
  commandChan <- newBroadcastTChanIO

  workers <- forM [1..caps] $ \_ -> do
    comchan <- atomically (dupTChan commandChan)
    async (tweetInfo comchan tweetQueue resultQueue)

  stats <- do
    comchan <- atomically $ dupTChan commandChan
    async $ statsProc comchan resultQueue

  twit <- async $ twitProc twInfo tweetQueue

  reportLoop commandChan

  cancel twit -- works
  mapM_ wait (stats : workers)
