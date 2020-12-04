{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DuplicateRecordFields #-}

module GitHub where

import           Control.Monad       (mzero)
import           Data.Aeson
import           Data.Proxy
import           Data.Text
import           GHC.Generics
import           Network.HTTP.Client (defaultManagerSettings, newManager)
import           Servant.API
import           Servant.Client

type Username  = Text
type UserAgent = Text
type Reponame  = Text

data GitHubUser =
  GitHubUser { name  :: Text
             , followers :: Int
             , following :: Int
             } deriving (Generic, FromJSON, Show)


type GitHubAPI = "users" :> Header  "user-agent" UserAgent
                         :> Capture "username" Username  :> Get '[JSON] GitHubUser



gitHubAPI :: Proxy GitHubAPI
gitHubAPI = Proxy

getUser :: Maybe UserAgent -> Username -> ClientM GitHubUser


getUser  = client gitHubAPI
