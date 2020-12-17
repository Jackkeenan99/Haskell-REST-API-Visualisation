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
      putStrLn $ "Error getting user  " ++ show err
     Right res ->  do
      putStrLn $ "\nUser details->  " ++ show res



      (SC.runClientM (GH.getRepos (Just "haskell-app") auth name) =<< env) >>= \case
         Left err -> do
          putStrLn $ "Error getting user repos " ++ show err
         Right rees -> do
          putStrLn $ "\nList of Repositories->  " ++
             intercalate ", " (map (\(GH.GitHubRepo repo) -> unpack repo) rees)



          (partitionEithers <$> mapM (getInfo auth name) rees) >>= \case
            ([], contribs) -> putStrLn $ " \nGitHub Repo Info:\n" ++
             intercalate "\n"  (map (\(GH.GitHubRepoInfo r n c d l f) ->
             show r ++ " subscribers:" ++  show n ++ " watchers:"
              ++ show c ++ " size:" ++ show d ++ " open issues:" ++ show l
              ++ " forks:" ++ show f) contribs)
            (ers, _)-> do
               putStrLn $ "Error getting repository information" ++ show ers



          (partitionEithers <$> mapM (getContributors auth name) rees) >>= \case
              ([], contribss) -> do putStrLn $ "\n\n"  ++  intercalate "\n"  (map (\(GH.ContributerLogin l c)
                                      -> show l ++ " Commits:" ++ show c) . groupContributors $ concat contribss)
                                    (partitionEithers <$> (mapM (getRepoUsers auth) . groupContributors $ concat contribss)) >>= \case
                                      ([], contribs) -> putStrLn $ "\n\n" ++ intercalate "\n"
                                                          (map (\(GH.GitHubUser n fl fg r) -> show n ++ " Followers:"
                                                            ++ show fl ++ " Following:" ++ show fg ++ " Repos" ++ show r) contribs)
                                      (ers, _)-> do putStrLn $ "Error getting contributer profile information  " ++ show ers
              (ers, _)-> do
                 putStrLn $ "Error getting contributers:" ++ show ers



  where env :: IO SC.ClientEnv
        env = do
          manager <- newManager tlsManagerSettings
          return $ SC.mkClientEnv manager (SC.BaseUrl SC.Http "api.github.com" 80 "")


        getInfo:: BasicAuthData -> GH.Username -> GH.GitHubRepo -> IO (Either SC.ClientError GH.GitHubRepoInfo)
        getInfo auth name (GH.GitHubRepo repo) = SC.runClientM (GH.getRepoInfo (Just "haskell-app") auth name repo) =<< env


        getContributors:: BasicAuthData -> GH.Username -> GH.GitHubRepo -> IO (Either SC.ClientError [GH.ContributerLogin])
        getContributors auth name (GH.GitHubRepo repo) = SC.runClientM (GH.getContributors (Just "haskell-app") auth name repo) =<< env


        getRepoUsers:: BasicAuthData -> GH.ContributerLogin -> IO (Either SC.ClientError GH.GitHubUser)
        getRepoUsers auth (GH.ContributerLogin n _) = SC.runClientM (GH.getUser (Just "haskell-app") auth n) =<< env


        groupContributors :: [GH.ContributerLogin] -> [GH.ContributerLogin]
        groupContributors  = sortBy (\(GH.ContributerLogin _ c1) (GH.ContributerLogin _ c2) -> compare c1 c2) .
                             map mapfn .
                             groupBy (\(GH.ContributerLogin l1 _) (GH.ContributerLogin l2 _) -> l1 == l2)
         where mapfn :: [GH.ContributerLogin] -> GH.ContributerLogin

               mapfn xs@((GH.ContributerLogin l _):_) = GH.ContributerLogin l .sum $
                                                       map (\(GH.ContributerLogin _ c) -> c)  xs
