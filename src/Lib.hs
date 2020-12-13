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

someFunc :: IO ()
someFunc = do
  putStrLn "Let's try a GitHubCall"
  (rName:rep:_) <- getArgs
  putStrLn $ "name is " ++ rName
  putStrLn $ "repo is " ++ rep
  let n = pack rName
  testGitHubCall n $ pack rep
  putStrLn "end."


testGitHubCall :: Text -> Text -> IO ()
testGitHubCall name rep  =
  (SC.runClientM (GH.getUser (Just "haskell-app") name) =<< env) >>= \case

     Left err -> do
      putStrLn $ "heuston, we have a problem: " ++ show err
     Right res -> do
      putStrLn $ "the votes of the github jury are " ++ show res

      (SC.runClientM (GH.getRepo  (Just "haskell-app") name rep) =<< env) >>= \case
        Left err -> do
           putStrLn $ "heuston, we have a problem: " ++ show err
        Right res' -> do
           putStrLn $ "the votes of the github jury are " ++ show res'





  where env :: IO SC.ClientEnv
        env = do
          manager <- newManager tlsManagerSettings
          return $ SC.mkClientEnv manager (SC.BaseUrl SC.Http "api.github.com" 80 "")
