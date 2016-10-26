{-# LANGUAGE OverloadedStrings, TypeApplications #-}

module Main where

import MockLoginPage.API
import Servant
import Network.Wai.Handler.Warp
import Data.Text (Text)

server :: Server MockApi
server u = return ("hohooo" :: Text)

main :: IO ()
main = run 8081 (serve (Proxy @MockApi) server)

-- invoke with
-- curl -X POST -d '43' -H 'Accept: application/json' -H 'Content-type: application/json' http://localhost:8081/auth

