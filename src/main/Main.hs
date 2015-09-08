{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import OAuth
import TwitterPipes
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Twitter
import Control.Monad.Trans
import Data.Configurator
import Data.Configurator.Types (Config)

loadCreds :: Config -> IO TwitterCredential
loadCreds conf = do
  token <- require conf "oauth_token" 
  secret <- require conf "oauth_token_secret"
  userId <- require conf "user_id"
  screenName <- require conf "screen_name"
  return $ twitterCredential token secret userId screenName

loadOAuth :: Config -> IO OAuth
loadOAuth conf = do
  key <- require conf "key"
  secret <- require conf "secret"
  return $ twitterOAuth key secret

main :: IO ()
main = do
  (conf, _) <- autoReload autoConfig [Required "$(HOME)/twitter.cfg"]
  myOAuth <- loadOAuth conf
  myCreds <- loadCreds conf
  runTwitterT myOAuth myCreds "" $ do
    tweet "Hello from Haskell."
    liftIO $ putStrLn "Done"
  --manager <- newManager tlsManagerSettings
  --creds <- credsInteraction myOAuth manager
  --print creds
  

postTwoTweets :: MonadIO m => TwitterT s m ()
postTwoTweets = do
  tweet "First tweet"
  wait 5
  tweet "Second tweet"
  liftIO $ putStrLn "Done"
