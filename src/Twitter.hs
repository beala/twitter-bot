{-# LANGUAGE DeriveFunctor, ScopedTypeVariables, OverloadedStrings #-}

module Twitter
( Twitter(..)
, TwitterT
, wait
, tweet
, example
, printInterp
, runTwitterT
) where

import Control.Monad.Trans.Free
import Web.Twitter.Types
import qualified Data.Text as T
-- import Data.Functor.Identity
import Control.Monad.Reader
import Control.Monad.Catch
-- import Control.Monad.IO.Class
import Control.Concurrent (threadDelay)
import OAuth

data TwitterTEnv = TwitterTEnv { twitterEnvCreds :: TwitterCredential
                               , twitterEnvOAuth :: OAuth
                               , twitterEnvStatus :: Maybe Status
                               }

type TwitterT s m r = FreeT Twitter (ReaderT s m) r

data Twitter r = Wait Int r
               | Tweet T.Text r
               | DM T.Text r
               deriving (Functor)


tweet :: Monad m => T.Text -> TwitterT s m ()
tweet t = liftF $ Tweet t ()

type Second = Int
wait :: Monad m => Second -> TwitterT s m ()
wait i = liftF $ Wait (secToMicroSec i) ()
  where secToMicroSec = (*) 1000000

getTweet :: Monad m => TwitterT s m s
getTweet = ask

example :: (Show s, MonadIO m) => TwitterT s m ()
example = do
  wait 1
  t <- getTweet
  liftIO $ putStrLn ("Got tweet: " ++ (show t))
  tweet "test test test"
  wait 1
  tweet "test test"
  liftIO $ putStrLn "Done"

printInterp :: MonadIO m => s -> TwitterT s m () -> m ()
printInterp status twitterT = runReaderT (iterT tearDown twitterT) status
  where tearDown (Wait delay next) = do
          liftIO $ putStrLn $ "Waiting " ++ (show delay)
          next
        tearDown (Tweet msg next) = do
          liftIO $ putStrLn $ "Tweeting " ++ (show msg)
          next
        tearDown (DM msg next) = do
          liftIO $ putStrLn $ "DMing " ++ (show msg)
          next

runTwitterT ::
  (MonadIO m, MonadThrow m) =>
  OAuth ->
  TwitterCredential ->
  s -> 
  TwitterT s m () ->
  m ()

runTwitterT oauth twitterCreds status twitterT =
  runReaderT (iterT tearDown twitterT) status
  where
    tearDown (Wait i r) = do
      liftIO $ threadDelay i
      r
    tearDown (Tweet s r) = do
      req <- liftIO $ updateReq oauth twitterCreds (UpdateReqOptions s Nothing)
      resp <- liftIO $ makeNonStreamingRequest req
      liftIO $ print resp
      r
    tearDown (DM s r) = undefined
