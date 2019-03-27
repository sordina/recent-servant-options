{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}


module Main where

import Data.Time (UTCTime, getCurrentTime)
import Servant.API
import Servant.Server
import GHC.Generics
import Data.Aeson
import Data.Ord
import Safe
import Data.Text as T (unpack)
import Data.List
import Data.Proxy
import Network.Wai.Handler.Warp
import Control.Monad.IO.Class
import Network.Wai.Middleware.Servant.Options

data SortBy = Age | Name deriving (Generic, Show, Read)

instance FromJSON SortBy

instance FromHttpApiData SortBy
  where
    parseUrlPiece x = case readMay (T.unpack x)
      of Nothing  -> error $ "Something went wrong... Couldn't decode SortBy parameter: " ++ show x
         Just  y  -> Right y

data User = User
  { name :: String
  , age :: Int
  , email :: String
  , registration_date :: UTCTime
  } deriving Generic

testUser :: IO User
testUser = do
  t <- getCurrentTime
  return $ User "lyndon" 35 "maydwell@gmail.com" t

instance ToJSON User

type UserAPI = "users" :> QueryParam "sortby" SortBy :> Get '[JSON] [User]
          :<|> Get '[PlainText, JSON] ()

instance MimeRender PlainText () where
  mimeRender _ () = "text/plain"

server1 :: MonadIO m => Maybe SortBy -> m [User]
server1 a = do
  u1 <- liftIO testUser
  u2 <- liftIO testUser
  respond a [u1,u2]

respond :: Monad m => Maybe SortBy -> [User] -> m [User]
respond Nothing  _ = error "must supply sortby parameter"
respond (Just y) l = return $ sortBy (ordering y) l

ordering :: SortBy -> User -> User -> Ordering
ordering Age  = comparing age
ordering Name = comparing name

userAPI :: Proxy UserAPI
userAPI = Proxy

simple :: Servant.Server.Handler ()
simple = return ()

app1 :: Application
app1 = (provideOptions userAPI) $ serve userAPI (server1 :<|> simple)

main :: IO ()
main = do
  putStrLn "running on port 8081"
  run 8081 app1
