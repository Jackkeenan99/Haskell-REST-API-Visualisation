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


data GitHubTopics =
   GitHubTopics { subscribers_count :: Int
                  ,size :: Int
                  ,watchers_count :: Int
                } deriving (Generic, FromJSON, Show)

data GitHubRepo     =
      GitHubRepo     { name  :: Text
                    } deriving (Generic, FromJSON, Show)


data GitHubRepoInfo =
      GitHubRepoInfo  { name :: Text
                       ,subscribers_count :: Int
                      } deriving (Generic, FromJSON, Show)









type GitHubAPI = "users" :> Header  "user-agent" UserAgent
                         :> BasicAuth "github" Int
                         :> Capture "username" Username  :> Get '[JSON] GitHubUser
            :<|> "users" :> Header  "user-agent" UserAgent
                         :> BasicAuth "github" Int
                         :> Capture "username" Username  :> "repos" :>  Get '[JSON] [GitHubRepo]
            :<|> "repos" :> Header  "user-agent" UserAgent
                         :> BasicAuth "github" Int
                         :> Capture "owner" Username  :> Capture "repo" Reponame :> Get '[JSON] GitHubTopics
            :<|> "repos" :> Header  "user-agent" UserAgent
                         :> BasicAuth "github" Int
                         :> Capture "owner" Username
                         :> Capture "repo" Reponame :>  Get '[JSON] GitHubRepoInfo



gitHubAPI :: Proxy GitHubAPI
gitHubAPI = Proxy

getUser :: Maybe UserAgent -> BasicAuthData -> Username -> ClientM GitHubUser
getR    :: Maybe UserAgent -> BasicAuthData -> Username -> ClientM [GitHubRepo]
getRepo :: Maybe UserAgent -> BasicAuthData -> Username -> Reponame -> ClientM GitHubTopics
getRepoInfo :: Maybe UserAgent -> BasicAuthData -> Username -> Reponame -> ClientM GitHubRepoInfo


getUser :<|> getR :<|> getRepo :<|> getRepoInfo = client gitHubAPI
