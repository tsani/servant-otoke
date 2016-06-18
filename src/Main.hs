{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import Network.Wai ( Application, requestHeaders )
import Network.Wai.Handler.Warp ( run )
import Servant
import Servant.Server.Internal.RoutingApplication

data Otoke

instance HasServer sublayout context
  => HasServer (Otoke :> sublayout) context where

  type ServerT (Otoke :> sublayout) m = () -> ServerT sublayout m

  route Proxy context subserver =
    route (Proxy :: Proxy sublayout) context (addAuthCheck subserver go) where
      go = withRequest $ \req -> do
        case parseHeaderMaybe =<< lookup "Authorization" (requestHeaders req) of
          Nothing -> delayedFail err401
          Just h -> if h `elem` pws then pure () else delayedFail err401

      pws :: [T.Text]
      pws = ("oToke " `T.append`) <$>
        [ "hello"
        , "world"
        ]

      parseHeaderMaybe :: FromHttpApiData a => BS.ByteString -> Maybe a
      parseHeaderMaybe = eitherMaybe . parseHeader where
        eitherMaybe :: Either e a -> Maybe a
        eitherMaybe e = case e of
          Left _ -> Nothing
          Right x -> Just x


type MyAPI
  = "unprotected" :> Get '[PlainText] String
  :<|> "protected" :> Otoke :> Get '[PlainText] String

myServer :: Server MyAPI
myServer
  = pure "not secret"
  :<|> const (pure "secret")

app :: Application
app = serve (Proxy :: Proxy MyAPI) myServer

main :: IO ()
main = run 8081 app
