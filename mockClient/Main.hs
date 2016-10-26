{-# LANGUAGE NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings #-}
{-# LANGUAGE RecursiveDo, ScopedTypeVariables, ViewPatterns, TypeApplications, ExplicitForAll, PartialTypeSignatures #-}



-- {-# OPTIONS_GHC -fdefer-typed-holes #-}

module Main where

import ClassyPrelude
import Reflex
import Reflex.Dom
import Data.Proxy
import Servant.API
import Servant.Reflex

import Data.String.Conv    (toS)
import Text.Email.Validate (toByteString, validate, EmailAddress(..))

import MockLoginPage.API

-- invokeAPI = client (Proxy @MockApi) Proxy host
--   where host = constDyn $ BaseUrl Http "localhost" 8080
-- host = constDyn $ BaseUrl Http "localhost" 8080


--------------------------------------------------------------------------------

{- Note: The structure of the application

As you can see the structure of the main function is quite linear, and
corresponds to the high level structure of the feature. The most important
functions are validateInput and notifyLogin, defined below.

The htmlHead function provides some styling from a cdn for convenience.
-}
htmlHead :: forall t m. DomBuilder t m => m ()
htmlHead = do
  elAttr "meta" ("charset" =: "utf-8") (pure ())
  elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1.0") (pure ())
  el "title" (text "ui-mockup")
  styleSheet "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
  styleSheet "http://code.ionicframework.com/ionicons/2.0.1/css/ionicons.min.css"
  styleSheet "file:///home/carlo/code/haskell/goa/mockups/mockLoginPage/mockClient/assets/css/Login-Form-Clean.css"
  styleSheet "file:///home/carlo/code/haskell/goa/mockups/mockLoginPage/mockClient/assets/css/styles.css"
  where
    styleSheet addr = elAttr "link" ("rel"  =: "stylesheet" <> "href" =: addr) (return ())

main = mainWidgetWithHead htmlHead body
-- main = mainWidgetWithHead htmlHead bodyDressed

api :: Proxy MockApi
api = Proxy

body :: forall t m. MonadWidget t m => m ()
body = do
  divClass "login-clean" $ do
    elAttr "form" ("method" =: "post") $ do
      elClass "h2" "sr-only" (text "Login Form")
      divClass "illustration" $ elClass "i" "icon ion-ios-navigate" (pure ())
      mailInput <- divClass "form-group" $ elAttr "input" ("class" =: "form-control" <> "type" =: "email"
                                            <> "name" =: "email" <> "placeholder" =: "Email") (return ())
      passInput <- divClass "form-group" $ elAttr "input" ("class" =: "form-control" <> "type" =: "password"
                                            <> "name" =: "password" <> "placeholder" =: "Password") (pure ())
      bt <- divClass "form-group" $ elAttr "button" ("class" =: "btn btn-primary btn-block" <> "type" =: "submit") (pure ())
      elAttr "a" ("href" =: "#" <> "class" =: "forgot") $ text "Forgot your email or password?"
      elAttr "script" ("src" =: "assets/js/jquery.min.js") (pure ())
      elAttr "script" ("src" =: "assets/bootstrap/js/bootstrap.min.js") (pure ())

bodyDressed :: forall t m. MonadWidget t m => m ()
bodyDressed = do
  let url = BaseFullUrl Http "localhost" 8081 ""
      (invokeAPI :<|> _) = client (Proxy @MockApi) (Proxy @m) (constDyn url)
  divClass "login-clean" $ do
    el "form" $ do
      elClass "h2" "sr-only" (text "Login Form")
      divClass "illustration" $ elClass "i" "icon ion-ios-navigate" (pure ())
      mail <- _textInput_value <$> (divClass "form-group" $ elAttr "input" ("class" =: "form-control" <> "type" =: "email"
                                            <> "name" =: "email" <> "placeholder" =: "Email") (textInput def))
      pass <- _textInput_value <$> (divClass "form-group" $ elAttr "input" ("class" =: "form-control" <> "type" =: "password"
                                            <> "name" =: "password" <> "placeholder" =: "Password") (textInput def))
      let userResult = liftA2 (User) mail pass
      button <- divClass "form-group" $ elAttr "button" ("class" =: "btn btn-primary btn-block" <> "type" =: "submit") (button "Log in")
      elAttr "a" ("href" =: "#" <> "class" =: "forgot") $ text "Forgot your email or password?"
      apiResponse <- invokeAPI (Right <$> userResult) button
      r <- holdDyn "Waiting" $ fmap parseR apiResponse
      dynText r

bodySimple :: forall t m. MonadWidget t m => m ()
bodySimple = do
  let url = BaseFullUrl Http "localhost" 8081 ""
      (invokeAPI :<|> _) = client (Proxy @MockApi) (Proxy @m) (constDyn url)
  mail <- _textInput_value <$> textInput def
  pass <- _textInput_value <$> textInput def
  let userResult = liftA2 (User) mail pass
  button <- button "Log in"
  apiResponse <- invokeAPI (Right <$> userResult) button
  r <- holdDyn "Waiting" $ fmap parseR apiResponse
  dynText r

parseR :: ReqResult Text -> Text
parseR (ResponseSuccess a b) = a
parseR (ResponseFailure a b) = "ResponseFailure: " <> a
parseR (RequestFailure s) = "RequestFailure: " <> s
