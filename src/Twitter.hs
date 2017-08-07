{-# LANGUAGE OverloadedStrings, LambdaCase, TemplateHaskell #-}
module Twitter where

import Web.Twitter.Conduit hiding (count)
-- import Web.Twitter.Conduit.Parameters (count)
import Web.Twitter.Types

import Data.Conduit as CL
import Data.Conduit.List as CL

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource

import Control.Concurrent.STM

import Types

data SampleStream
sampleStream :: APIRequest SampleStream StreamingAPI
sampleStream = APIRequestGet "https://stream.twitter.com/1.1/statuses/sample.json" []

-- TODO When oauth info is wrong there is no error signaled whatsoever. Really irritating.
twitProc :: TWInfo -> TBQueue TweetInfo -> IO ()
twitProc twInfo twq = do
    mgr <- newManager tlsManagerSettings
    runResourceT $ do
--        src <- stream twInfo mgr userstream
        src <- stream twInfo mgr sampleStream
        -- src $$+- CL.mapM_ $ liftIO . printTL
        src $$+- CL.mapM_ $ liftIO . processTweet twq

        -- There is a bit of contention on really high load, may be better to have multiple tweet queues.
        -- src $$+- CL.mapM_ $ (\tw -> liftIO $ Prelude.mapM_ (processTweet twq) (Prelude.replicate 100 tw))

processTweet :: TBQueue TweetInfo -> StreamingAPI -> IO ()
processTweet twq (SStatus s) = atomically $ writeTBQueue twq twinfo
  where
    getEntities f = maybe [] (fmap entityBody . f) (statusEntities s)
    twinfo = TweetInfo (statusText s) (getEntities enHashTags)
              (getEntities enURLs) (getEntities enMedia)

-- Should do something with retweets?
processTweet _ _ = return ()


-- printTL :: StreamingAPI -> IO ()
-- printTL (SStatus s) = do
--   print (statusText s)
--   print (statusEntities s) -- hashtags, urls, media
-- printTL s = print s
-- printTL (SRetweetedStatus s) = print s
