{-# LANGUAGE TypeApplications #-}

module Main where

import MockLoginPage.API
import Servant
import Network.Wai.Handler.Warp

server :: Server MockApi
server u = return 42

main :: IO ()
main = run 8081 (serve (Proxy @MockApi) server)

-- invoke with
-- curl -X POST -d '43' -H 'Accept: application/json' -H 'Content-type: application/json' http://localhost:8081/auth

