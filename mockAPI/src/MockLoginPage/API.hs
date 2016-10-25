{-# LANGUAGE DataKinds, DeriveGeneric, TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module MockLoginPage.API where

import Data.Aeson
import Data.Text
import GHC.Generics
import Servant.API

data User = User
  { mail     :: Text
  , password :: Text
  } deriving (Show, Generic)

instance FromFormUrlEncoded User where
  fromFormUrlEncoded inputs = undefined
    User <$> lkp "mail" <*> lkp "password"

    where lkp input_label = case lookup input_label inputs of
                 Nothing -> Left $ "error in FromFormUrlEncoded"
                 Just v  -> Right v

instance ToJSON User
instance FromJSON User

type MockApi = "auth" :> ReqBody '[FormUrlEncoded] User :> Post '[JSON] Int
