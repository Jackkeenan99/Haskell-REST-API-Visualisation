{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields     #-}

module Lib
    ( someFunc
    ) where

import qualified GitHub as GH
import qualified Servant.Client               as SC
import           Network.HTTP.Client          (newManager)
import           Network.HTTP.Client.TLS      (tlsManagerSettings)
import           System.Environment           (getArgs)
import Data.Text hiding (map,intercalate, groupBy, concat)
import Data.List (intercalate, groupBy, sortBy)
import Data.Either
import           Servant.API                (BasicAuthData (..))
import Data.ByteString.UTF8 (fromString)

someFunc :: IO ()
someFunc = do
  putStrLn "Let's try a GitHubCall"
  (rName:user:token:_) <- getArgs
  putStrLn $ "name is " ++ rName
  putStrLn $ "github account for API call is " ++ user
  putStrLn $ "github token for api call is " ++ token

  let auth = BasicAuthData (fromString user) (fromString token)

  testGitHubCall auth $ pack rName
  -- $ pack rep
  putStrLn "end."




testGitHubCall :: BasicAuthData -> Text -> IO ()
testGitHubCall auth name =
  (SC.runClientM (GH.getUser (Just "haskell-app") auth name) =<< env) >>= \case

     Left err -> do
      putStrLn $ "ERROR: " ++ show err
     Right res ->  do
      putStrLn $ "\nUser details->  " ++ show res

      (SC.runClientM (GH.getR (Just "haskell-app") auth name) =<< env) >>= \case
         Left err -> do
          putStrLn $ "ERROR: " ++ show err
         Right rees -> do
          putStrLn $ "\nList of Repositories->  " ++
             intercalate "," (map (\(GH.GitHubRepo repo) -> unpack repo) rees)

          (partitionEithers <$> mapM (getInfo auth name) rees) >>= \case

            ([], contribs) -> putStrLn $ " \nGitHub Repo Info:\n" ++
             intercalate "\n"  (map (\(GH.GitHubRepoInfo r n c d l f) ->
             show r ++ " subscribers:" ++  show n ++ " watchers:"
              ++ show c ++ " size:" ++ show d ++ " open issues:" ++ show l
              ++ " forks:" ++ show f) contribs)

            (ers, _)-> do
               putStrLn $ "heuston, we have a problem (getting contributors):" ++ show ers


          (partitionEithers <$> mapM (getContributors auth name) rees) >>= \case
              ([], contribs) -> putStrLn $ "\n\n"  ++  intercalate "\n"  (map (\(GH.ContributerLogin l c)
               -> show l ++ " " ++ show c) $ concat contribs)


              (ers, _)-> do
                 putStrLn $ "heuston, we have a problem (getting contributors):" ++ show ers




  where env :: IO SC.ClientEnv
        env = do
          manager <- newManager tlsManagerSettings
          return $ SC.mkClientEnv manager (SC.BaseUrl SC.Http "api.github.com" 80 "")

        getInfo:: BasicAuthData -> GH.Username -> GH.GitHubRepo -> IO (Either SC.ClientError GH.GitHubRepoInfo)
        getInfo auth name (GH.GitHubRepo repo) = SC.runClientM (GH.getRepoInfo (Just "haskell-app") auth name repo) =<< env


        getContributors:: BasicAuthData -> GH.Username -> GH.GitHubRepo -> IO (Either SC.ClientError [GH.ContributerLogin])
        getContributors auth name (GH.GitHubRepo repo) = SC.runClientM (GH.getContributors (Just "haskell-app") auth name repo) =<< env


        --getRepoUsers:: BasicAuthData -> GH.Username -> GH.GitHubRepo -> IO (Either SC.ClientError GH.GitHubUser)
