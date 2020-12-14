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
     Right res -> do
      putStrLn $ "User details->  " ++ show res
      (SC.runClientM (GH.getR (Just "haskell-app") auth name) =<< env) >>= \case

         Left err -> do
          putStrLn $ "ERROR: " ++ show err
         Right rees -> do
          putStrLn $ "List of Repositories->  " ++
             intercalate ", " (map (\(GH.GitHub repo) -> unpack repo) rees)



        --  (partitionEithers <$> mapM (getCommits auth name) rees) >>= \case

          -- ([], contribs) -> putStrLn $ " contributors are: " ++ show contribs
           --(ers, _)-> do
            --  putStrLn $ "heuston, we have a problem (getting contributors):" ++ show ers





        --  (SC.runClientM (GH.getRepo  (Just "haskell-app") name rep) =<< env) >>= \case
          --  Left err -> do
            -- putStrLn $ "ERROR: " ++ show err
            --Right res' -> do
            --  putStrLn $ "Repo details-> " ++ show res'





  where env :: IO SC.ClientEnv
        env = do
          manager <- newManager tlsManagerSettings
          return $ SC.mkClientEnv manager (SC.BaseUrl SC.Http "api.github.com" 80 "")

        getCommits:: BasicAuthData -> GH.Username -> GH.GitHub -> IO (Either SC.ClientError [GH.GitHubCommit])
        getCommits auth name (GH.GitHub repo) = SC.runClientM (GH.getCommits (Just "haskell-app") auth name repo) =<< env
