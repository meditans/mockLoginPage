{-# LANGUAGE OverloadedStrings, TypeApplications #-}

module Main where

import MockLoginPage.API
import Servant
import Network.Wai.Handler.Warp
import Data.Text (Text)

server :: Server MockApi
server = (\_ -> return ("hohooo" :: Text))
      :<|> serveJS
  where
    authenticate u = if mail u == "meditans@gmail.com" && password u == "securepassword"
                     then "Authenticated"
                     else "Not Authenticated"
    serveJS = serveDirectory  "/home/carlo/code/haskell/goa/mockups/mockLoginPage/mockClient/.stack-work/dist/x86_64-linux/Cabal-1.24.0.0_ghcjs/build/mockClient/mockClient.jsexe/"

main :: IO ()
main = run 8081 (serve (Proxy @MockApi) server)

-- invoke with
-- curl -X POST -d '43' -H 'Accept: application/json' -H 'Content-type: application/json' http://localhost:8081/auth

