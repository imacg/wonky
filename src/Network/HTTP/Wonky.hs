{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, ExistentialQuantification, TypeApplications, DerivingVia, RankNTypes, TupleSections, ScopedTypeVariables #-}
module Network.HTTP.Wonky
  ( get
  , Decoder
  , ignore
  , L.responseBody
  , http
  , https
  , Options
  , (/:)
  , (=:)
  , (?)
  , URL
  , Raw
  , raw
  , flag
  , withHeader
  , userAgent
  ) where

import Data.Coerce
import Control.Monad ((>=>))
import UnliftIO.Exception (Exception, SomeException)
import Data.Functor (($>))
import Data.Default
import qualified Data.Maybe as M
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BSL
import Data.Kind
import qualified Network.HTTP.Client as L
import qualified Network.HTTP.Client.TLS as LT
import qualified Network.HTTP.Types as W
import qualified Network.HTTP.Types.Method as HTM
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T
import Control.Monad.IO.Class
import qualified Data.Text.Encoding as TE

class ToParam a where
  toParam :: a -> T.Text

class Decoder t where
  type Result t :: Type
  extractResponse :: t -> L.Response B.ByteString -> IO (Result t)
  acceptHeader :: t -> Maybe B.ByteString
  acceptHeader _ = Nothing
  
data URL = URL { urlScheme :: Scheme
               , urlHost :: T.Text
               , urlPathParts :: [T.Text]
               , urlQuery :: Query
               , urlPort :: Int
               } deriving (Show)

newtype Options (m :: Type -> Type) = Options [Option m] deriving (Semigroup, Monoid, Default)
newtype Option (m :: Type -> Type) = Finalize (L.Request -> m L.Request)

data Scheme = Https | Http deriving (Show, Eq)
newtype Query = Query [(T.Text, Maybe T.Text)]
              deriving (Show, Semigroup, Monoid)

https :: T.Text -> URL
https host = URL Https host [] mempty 443

http :: T.Text -> URL
http host = URL Http host [] mempty 80

(?) :: URL -> Query -> URL
url ? query = url { urlQuery = urlQuery url <> query }

infixl 1 ?

(/:) :: URL -> T.Text -> URL
(/:) url piece = url { urlPathParts = urlPathParts url <> [piece] }

(=:) :: (ToParam p) => T.Text -> p -> Query
name =: p = Query [(name, Just $ toParam p)]

flag :: T.Text -> Query
flag name = Query [(name, Nothing)]

instance ToParam T.Text where
  toParam = id
  
instance ToParam Int where
  toParam = T.pack . show

data Ignore = Ignore

ignore :: Ignore
ignore = Ignore

data Raw = Raw

raw :: Raw
raw = Raw

data HttpException = DecodeError SomeException deriving (Show)

instance Exception HttpException

instance Decoder Raw where
  type Result Raw = B.ByteString
  acceptHeader _ = Nothing
  extractResponse _ reader = pure $ L.responseBody reader

instance Decoder Ignore where
  type Result Ignore = ()
  extractResponse _ _ = pure ()

withHeader' :: (Monad m) => W.HeaderName -> T.Text -> Options m
withHeader' name value = finalizer $ \req ->
  pure $ req { L.requestHeaders = (name, TE.encodeUtf8 value) : L.requestHeaders req }

withHeader :: (Monad m) => T.Text -> T.Text -> Options m
withHeader name value = withHeader' (CI.mk $ TE.encodeUtf8 name) value

finalizer :: (L.Request -> m L.Request) -> Options m
finalizer f = Options [Finalize f]

userAgent :: Monad m => T.Text -> Options m
userAgent = withHeader "user-agent"

get :: (MonadIO m, Decoder d) => URL -> d -> Options m -> m (Result d)
get url decoder opts = do
  manager <- liftIO $ LT.getGlobalManager
  request <- mcompose finalizers baseRequest
  liftIO $ L.withResponse request manager (readBody >=> extractResponse decoder)
  where
    finalizers = coerce opts
    mcompose (m:ms) v = m v >>= mcompose ms
    mcompose _ v = pure v
    readBody response = do
      let reader = L.responseBody response
      body <- BSL.toStrict . BSL.fromChunks <$> L.brConsume reader
      pure $ response $> body
    convert (name, value) = (TE.encodeUtf8 name, TE.encodeUtf8 <$> value)
    renderQuery (Query items) = W.renderQuery True $ fmap convert items
    methodName = HTM.methodGet
    accept = (W.hAccept,) <$> M.maybeToList (acceptHeader decoder)
    baseRequest = L.defaultRequest { L.method = methodName
                                   , L.port = urlPort url
                                   , L.queryString = renderQuery $ urlQuery url
                                   , L.host = TE.encodeUtf8 $ urlHost url
                                   , L.path = TE.encodeUtf8 $ T.intercalate "/" $ urlPathParts url
                                   , L.secure = urlScheme url == Https
                                   , L.requestHeaders = accept ++ L.requestHeaders L.defaultRequest
                                   }
    
