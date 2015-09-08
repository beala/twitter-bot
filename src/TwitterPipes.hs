{-# LANGUAGE ScopedTypeVariables #-}

module TwitterPipes
( makeRequest
, parseStreaming
, printAndIgnoreErrors
, parseAndPrint
) where

import Pipes
import qualified Pipes.Prelude as P
import qualified Pipes.Parse as PP
import qualified Pipes.ByteString as PB
import Pipes.Aeson (DecodingError, decode)
import qualified Web.Twitter.Types as TT
import Pipes.HTTP
import Control.Monad (forever)

makeRequest ::
  MonadIO m =>
  Request ->
  (Producer PB.ByteString IO () -> IO ()) ->
  m ()
makeRequest req handler = do
  liftIO $ withManager tlsManagerSettings $ \m ->
    withHTTP req m (handler . responseBody)

parseStreaming ::
  Monad m =>
  Pipe PB.ByteString (Either DecodingError TT.StreamingAPI) m ()
parseStreaming = PP.parseForever_ decode

printAndIgnoreErrors ::
  MonadIO m =>
  Pipe (Either DecodingError TT.StreamingAPI) (TT.StreamingAPI) m ()
printAndIgnoreErrors = forever $ do
  resultOrError <- await
  either (liftIO . print) (yield) resultOrError

parseAndPrint ::
  Producer PB.ByteString IO () ->
  IO ()
parseAndPrint r = runEffect $
                  r >->
                  parseStreaming >->
                  printAndIgnoreErrors >->
                  P.print
