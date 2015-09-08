{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module OAuth
( twitterOAuth
, credsInteraction
, streamingTimelineReq
, streamingSampleReq
, updateReq
, twitterCredential
, makeNonStreamingRequest
, TwitterCredential
, Web.Authenticate.OAuth.OAuth
, UpdateReqOptions(..)
) where

import Web.Authenticate.OAuth

import Network.HTTP.Client
import Network.HTTP.Client.TLS

import Control.Monad.IO.Class
import Control.Monad.Catch

import qualified Data.ByteString as B (putStr, getLine, ByteString)
import qualified Data.ByteString.Char8 as B (pack)

import qualified Data.ByteString.Lazy as BL

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- Constructor not exported. Use fullCreds smart constructor
newtype TwitterCredential = TwitterCredential Credential
                          deriving (Show)

type AccessToken = B.ByteString
type AccessSecret = B.ByteString
type UserId = B.ByteString
type ScreenName = B.ByteString

twitterCredential ::
  AccessToken ->
  AccessSecret ->
  UserId ->
  ScreenName ->
  TwitterCredential
twitterCredential token secret user_id screen_name = TwitterCredential $ Credential $
  [ ("oauth_token", token)
  , ("oauth_token_secret", secret)
  , ("user_id", user_id)
  , ("screen_name", screen_name)
  , ("x_auth_expires", "0")
  ]

credsInteraction :: MonadIO m => OAuth -> Manager -> m TwitterCredential
credsInteraction twitterOAuthConfig manager = do
  tempToken            <- getTemporaryCredential twitterOAuthConfig manager
  let url              =  authorizeUrl twitterOAuthConfig tempToken
  oAuthVerifier        <- prompt $ B.pack $ url  ++ "\nPIN: "
  let credWithVerifier =  addOAuthVerifier oAuthVerifier tempToken
  fullCreds            <- getAccessToken twitterOAuthConfig credWithVerifier manager
  return $ TwitterCredential fullCreds

prompt :: MonadIO m => B.ByteString -> m B.ByteString
prompt p = liftIO $ do
  B.putStr p
  B.getLine

type ConsumerKey = B.ByteString
type ConsumerSecret = B.ByteString

twitterOAuth :: ConsumerKey -> ConsumerSecret -> OAuth
twitterOAuth key secret = def
                          { oauthServerName = "twitter"
                          , oauthRequestUri = "https://api.twitter.com/oauth/request_token"
                          , oauthAccessTokenUri = "https://api.twitter.com/oauth/access_token"
                          , oauthAuthorizeUri = "https://api.twitter.com/oauth/authorize"
                          , oauthConsumerKey = key
                          , oauthConsumerSecret = secret
                          , oauthSignatureMethod = HMACSHA1
                          , oauthCallback = Just "oob" -- PIN auth
                          }

addOAuthVerifier :: B.ByteString -> Credential -> Credential
addOAuthVerifier verifier cred = let oldC = unCredential cred
                                     newC = ("oauth_verifier", verifier) : oldC
                                     in
                                  Credential newC

streamingTimelineReq ::
  (MonadThrow m, MonadIO m) =>
  OAuth ->
  TwitterCredential ->
  m Request

streamingTimelineReq oauth (TwitterCredential creds) = do
  req <- parseUrl "https://userstream.twitter.com/1.1/user.json"
  signOAuth oauth creds req

streamingSampleReq ::
  (MonadThrow m, MonadIO m) =>
  OAuth ->
  TwitterCredential ->
  m Request

streamingSampleReq oauth (TwitterCredential creds) = do
  req <- parseUrl "https://stream.twitter.com/1.1/statuses/sample.json"
  signOAuth oauth creds req

data UpdateReqOptions = UpdateReqOptions { updateReqStatus :: T.Text
                                         , updateReqInReplyToId :: Maybe T.Text
                                         }

updateReq ::
  (MonadThrow m, MonadIO m) =>
  OAuth ->
  TwitterCredential ->
  UpdateReqOptions ->
  m Request

updateReq oauth (TwitterCredential creds) (UpdateReqOptions status inReplyTo) = do
  req <- parseUrl "https://api.twitter.com/1.1/statuses/update.json"
  let reqWithParams = urlEncodedBody (makeBody status inReplyTo) req
  signOAuth oauth creds reqWithParams
    where makeBody status (Just inReplyTo) = [ ("status", T.encodeUtf8 status)
                                             , ("in_reply_to_status_id", T.encodeUtf8 inReplyTo)
                                             ]
          makeBody status Nothing = [ ("status", T.encodeUtf8 status) ]

makeNonStreamingRequest ::
  (MonadIO m) =>
  Request ->
  m (Response BL.ByteString)

makeNonStreamingRequest req = do
  manager <- liftIO $ newManager tlsManagerSettings
  liftIO $ httpLbs req manager
