{-# LANGUAGE NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings #-}
{-# LANGUAGE RecursiveDo, ScopedTypeVariables, ViewPatterns, TypeApplications, ExplicitForAll #-}

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
  where
    styleSheet addr = elAttr "link" ("rel"  =: "stylesheet" <> "href" =: addr) (return ())

-- main = mainWidgetWithHead htmlHead body'
main = mainWidgetWithHead htmlHead bodySimple

api :: Proxy MockApi
api = Proxy

-- body :: forall t m. MonadWidget t m => m ()
-- body = do
--   url <- baseUrlWidget
--   let invokeAPI = client (Proxy @MockApi) (Proxy @m) url
--   divClass "login-clean" $ do
--     elAttr "form" ("method" =: "post") $ do
--       elClass "h2" "sr-only" (text "Login Form")
--       divClass "illustration" $ elClass "i" "icon ion-ios-navigate" (pure ())
--       mailInput <- divClass "form-group" $ elAttr "input" ("class" =: "form-control" <> "type" =: "email"
--                                             <> "name" =: "email" <> "placeholder" =: "Email") (textInput def)
--       passInput <- divClass "form-group" $ elAttr "input" ("class" =: "form-control" <> "type" =: "password"
--                                             <> "name" =: "password" <> "placeholder" =: "Password") (textInput def)
--       let mailResult = _textInput_value mailInput
--           passResult = _textInput_value passInput
--           userResult = liftA2 (User) mailResult passResult
--           eitherUser = fmap Right userResult

--       bt <- divClass "form-group" $ elAttr "button" ("class" =: "btn btn-primary btn-block" <> "type" =: "submit") (button "Log in")
--       apiResponse <- invokeAPI eitherUser bt
--       let parseR (ResponseSuccess a b) = if a then "Auth" else "No Auth"
--           parseR (ResponseFailure t b) = t
--           parseR (RequestFailure s) = s
--       r <- holdDyn "Waiting" $ fmap parseR apiResponse
--       dynText r
--       elAttr "a" ("href" =: "#" <> "class" =: "forgot") $ text "Forgot your email or password?"

-- body' :: forall t m. MonadWidget t m => m ()
-- body' = do
--   url <- baseUrlWidget
--   let invokeAPI = client (Proxy @MockApi) (Proxy @m) url
--   elClass "h2" "sr-only" (text "Login Form")
--   divClass "illustration" $ elClass "i" "icon ion-ios-navigate" (pure ())
--   mailInput <- divClass "form-group" $ elAttr "input" ("class" =: "form-control" <> "type" =: "email"
--                                         <> "name" =: "email" <> "placeholder" =: "Email") (textInput def)
--   passInput <- divClass "form-group" $ elAttr "input" ("class" =: "form-control" <> "type" =: "password"
--                                         <> "name" =: "password" <> "placeholder" =: "Password") (textInput def)
--   let mailResult = _textInput_value mailInput
--       passResult = _textInput_value passInput
--       userResult = liftA2 (User) mailResult passResult
--       eitherUser = fmap Right userResult

--   dynText (prettyPrint <$> userResult)

--   bt <- divClass "form-group" $ elAttr "button" ("class" =: "btn btn-primary btn-block" <> "type" =: "submit") (button "Log in")
--   apiResponse <- invokeAPI eitherUser bt
--   let parseR (ResponseSuccess a b) = if a then "Auth" else "No Auth"
--       parseR (ResponseFailure a b) = a
--       parseR (RequestFailure s) = s
--   r <- holdDyn "Waiting" $ fmap parseR apiResponse
--   dynText r
--   elAttr "a" ("href" =: "#" <> "class" =: "forgot") $ text "Forgot your email or password?"

invokeAPI' :: forall t m. MonadWidget t m => m ()
invokeAPI' :<|> _ = client (Proxy @MockApi) (Proxy @m) url

bodySimple :: forall t m. MonadWidget t m => m ()
bodySimple = do
  url <- baseUrlWidget
  let (invokeAPI :<|> _) = client (Proxy @MockApi) (Proxy @m) url
  mailInput <- textInput def
  passInput <- textInput def
  let mailResult = _textInput_value mailInput
      passResult = _textInput_value passInput
      userResult = liftA2 (User) mailResult passResult
      eitherUser = fmap Right userResult
  dynText (prettyPrint <$> userResult)
  bt <- button "Log in"
  apiResponse <- invokeAPI eitherUser bt
  let parseR (ResponseSuccess a b) = a
      parseR (ResponseFailure a b) = "ResponseFailure: " <> a
      parseR (RequestFailure s) = "RequestFailure: " <> s
  r <- holdDyn "Waiting" $ fmap parseR apiResponse
  dynText r

prettyPrint :: User -> Text
prettyPrint u = unwords [mail u, password u]

-- format :: XhrResponse -> Text
-- format (XhrResponse a b c d e) = unwords . map show $ [a,b,c,d,e]


-------------------------
run :: forall t m. MonadWidget t m => m ()
run = do
  url <- baseUrlWidget

  el "br" (return ())
  let shownBaseUrl = fmap showBaseUrl url
  dynText shownBaseUrl
  el "br" (return ())

  -- Name the computed API client functions
  -- let getUnit = client api (Proxy :: Proxy m) url
  let invokeAPI = client (Proxy @MockApi) (Proxy @m) url

  el "div" $ do
    unitBtn  <- button "Get unit"
    mailInput <- divClass "form-group" $ elAttr "input" ("class" =: "form-control" <> "type" =: "email"
                                        <> "name" =: "email" <> "placeholder" =: "Email") (textInput def)
    passInput <- divClass "form-group" $ elAttr "input" ("class" =: "form-control" <> "type" =: "password"
                                        <> "name" =: "password" <> "placeholder" =: "Password") (textInput def)
    let mailResult = _textInput_value mailInput
        passResult = _textInput_value passInput
        userResult = liftA2 (User) mailResult passResult
        eitherUser = fmap Right userResult
    unitResponse <- invokeAPI eitherUser unitBtn

    let parseR (ResponseSuccess a b) = (tshow a <> showXhrResponse b)
        parseR (ResponseFailure a b) = (a <> showXhrResponse b)
        parseR (RequestFailure s) = s
    r <- holdDyn "Waiting" $ fmap parseR unitResponse
    dynText r

showXhrResponse :: XhrResponse -> Text
showXhrResponse (XhrResponse stat stattxt resp resptxt headers) =
  unlines ["stat: " <> tshow stat
          ,"stattxt: " <> stattxt]

showRB :: XhrResponseBody -> Text
showRB (XhrResponseBody_Default t) = tshow t
showRB (XhrResponseBody_Text t) = tshow t
showRB (XhrResponseBody_Blob t) = "<Blob>"
showRB (XhrResponseBody_ArrayBuffer t) = tshow t
