{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Servant.Client.Internal.WasmFetchClient where

import           Prelude ()
import           Prelude.Compat

import           Control.Concurrent
                 (newEmptyMVar, takeMVar, putMVar)
import           Control.Exception
                 (Exception)
import           Control.Monad.Catch
                 (MonadCatch, MonadMask, MonadThrow, bracket)
import           Control.Monad.Error.Class
                 (MonadError (..))
import           Control.Monad.IO.Class
                 (MonadIO (..))
import           Control.Monad.Reader
                 (MonadReader, ReaderT, asks, runReaderT)
import           Control.Monad.Trans.Except
                 (ExceptT, runExceptT)
import           Data.Coerce
                 (coerce)
import           Data.Bifunctor
                 (bimap)
import           Data.ByteString.Builder
                 (toLazyByteString, byteString)
import qualified Data.ByteString.Lazy              as L
import           Data.CaseInsensitive
                 (mk, original)
import           Data.Foldable
                 (toList)
import           Data.Functor.Alt
                 (Alt (..))
import           Data.Maybe
                 (fromMaybe)
import           Data.Proxy
                 (Proxy (..))
import qualified Data.Sequence                     as Seq
import           Data.Text
                 (Text)
import qualified Data.Text                         as T
import qualified Data.Text.Encoding                as TE
import           GHC.Generics
import           GHC.Wasm.Prim
                 (JSString(..), fromJSString, toJSString)
import           Network.HTTP.Media
                 (renderHeader)
import           Network.HTTP.Types
                 (Status, http11, mkStatus, urlEncodeBuilder)

import           Servant.Client.Core
import qualified Servant.Client.Internal.ByteString as B
import qualified Servant.Client.Internal.JS         as JS

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

data ClientEnv
   = ClientEnv
   { baseUrl :: BaseUrl
   }

data JSaddleConnectionError = JSaddleConnectionError
  deriving (Eq, Show)

instance Exception JSaddleConnectionError

-- | Default 'ClientEnv'
mkClientEnv :: BaseUrl -> ClientEnv
mkClientEnv burl = ClientEnv burl

instance Show ClientEnv where
  showsPrec prec (ClientEnv burl) =
    showParen (prec >= 11)
      ( showString "ClientEnv {"
      . showString "baseUrl = "
      . showsPrec 0 burl
      . showString "}"
      )

client :: HasClient ClientM api => Proxy api -> Client ClientM api
client api = api `clientIn` (Proxy :: Proxy ClientM)

newtype ClientM a = ClientM
  { fromClientM :: ReaderT ClientEnv (ExceptT ClientError IO) a }
  deriving ( Functor, Applicative, Monad, MonadIO, Generic
           , MonadReader ClientEnv, MonadError ClientError)
deriving instance MonadThrow IO => MonadThrow ClientM
deriving instance MonadCatch IO => MonadCatch ClientM
deriving instance MonadMask  IO => MonadMask  ClientM

-- | Try clients in order, last error is preserved.
instance Alt ClientM where
  a <!> b = a `catchError` const b

instance RunClient ClientM where
  throwClientError = throwError
#if MIN_VERSION_servant_client_core(0,18,1)
  runRequestAcceptStatus acceptStatuses r = do
    performRequest (fromMaybe [] acceptStatuses) r
#else
  runRequest r = do
    performRequest [] r
#endif

runClientM :: ClientM a -> ClientEnv -> IO (Either ClientError a)
runClientM cm env = runExceptT $ flip runReaderT env $ fromClientM cm

-- ---------------------------------------------------------------------------
-- Core request execution
-- ---------------------------------------------------------------------------

performRequest :: [Status] -> Request -> ClientM Response
performRequest _ req = do
  burl <- asks baseUrl

  let method  = TE.decodeUtf8Lenient (requestMethod req)
      url     = toUrl burl req

      acceptHdrs =
        map (\mt -> ("Accept", T.pack $ show $ renderHeader mt))
            (toList $ requestAccept req)
      ctHdr =
        maybe []
              (\(_, mt) -> [("Content-Type", T.pack $ show $ renderHeader mt)])
              (requestBody req)
      extraHdrs =
        map (\(k, v) -> ( TE.decodeUtf8Lenient (original k)
                        , TE.decodeUtf8Lenient v))
            (toList $ requestHeaders req)

  hdrsObj   <- liftIO $ JS.buildHeaders (acceptHdrs <> ctHdr <> extraHdrs)
  maybeBody <- liftIO $ case toBody req of
    Nothing   -> JS.js_null
    Just body -> B.sendToJS (L.toStrict body)

  -- bracket acquire  : create an AbortController
  -- bracket release  : abort() — cancels the in-flight request on any
  --                    abnormal exit, including throwError and exceptions;
  --                    a no-op if the fetch already settled normally
  -- bracket use      : fire the fetch, block on the MVar, decode the result
  bracket
    (liftIO JS.js_newAbortController)
    (liftIO . JS.js_abort)
    (\controller -> do
        signal <- liftIO $ JS.js_abortSignal controller
        waiter <- liftIO newEmptyMVar

        successCb <- liftIO $ JS.js_mkCallback $ putMVar waiter . Right
        errorCb   <- liftIO $ JS.js_mkCallback $ putMVar waiter . Left

        liftIO $ JS.js_fetch
          (coerce $ toJSString (T.unpack url))
          (coerce $ toJSString (T.unpack method))
          hdrsObj
          maybeBody
          signal
          successCb
          errorCb

        liftIO (takeMVar waiter) >>= either
          -- Non-2xx or network error
          (\errResp -> do
              resp <- liftIO $ readResponse errResp
              throwError $ mkFailureResponse burl req resp
          )
          -- 2xx
          (\okResp -> liftIO $ readResponse okResp)
    )
  where
    readResponse r = do
      status     <- JS.js_respStatus r
      statusText <- T.pack . fromJSString . coerce <$> JS.js_respStatusText r
      hdrs       <- JS.collectHeaders r
      bodyBuf    <- JS.js_respBody r
      body       <- L.fromStrict <$> B.getFromJS bodyBuf
      pure $ mkResponse status statusText hdrs body

-- ---------------------------------------------------------------------------
-- Response / URL helpers
-- ---------------------------------------------------------------------------

mkResponse :: Int -> Text -> [(Text, Text)] -> L.ByteString -> Response
mkResponse status statusText hdrs body =
  Response
  { responseStatusCode  = mkStatus status (TE.encodeUtf8 statusText)
  , responseHeaders     = Seq.fromList $
      map (\(k, v) -> (mk (TE.encodeUtf8 k), TE.encodeUtf8 v)) hdrs
  , responseHttpVersion = http11
  , responseBody        = body
  }

toUrl :: BaseUrl -> Request -> Text
toUrl burl request =
  let pathS  = requestPath request
      queryS = foldr
        (\(first, (k, v)) b ->
            (if first then "?" else "&")
            <> urlEncodeBuilder True k
            <> maybe mempty (("=" <>) . byteString) v
            <> b
        ) mempty $ zip (True : repeat False) $ toList (requestQueryString request)
  in  T.pack (showBaseUrl burl)
   <> TE.decodeUtf8Lenient (L.toStrict $ toLazyByteString $ pathS <> queryS)

mkFailureResponse :: BaseUrl -> Request -> Response -> ClientError
mkFailureResponse burl request =
    FailureResponse (bimap (const ()) f request)
  where
    f b = (burl, L.toStrict $ toLazyByteString b)

toBody :: Request -> Maybe L.ByteString
toBody request = case requestBody request of
  Nothing                        -> Nothing
  Just (RequestBodyLBS "", _)    -> Nothing
  Just (RequestBodyLBS x,  _)    -> Just x
  Just (RequestBodyBS  "", _)    -> Nothing
  Just (RequestBodyBS  x,  _)    -> Just $ L.fromStrict x
  Just (RequestBodySource _, _)  -> error "RequestBodySource isn't supported"
