{-# LANGUAGE ExplicitForAll, NoImplicitPrelude, NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings, RecursiveDo, ScopedTypeVariables          #-}
{-# LANGUAGE TypeApplications                                             #-}

module Main where

import ClassyPrelude
import Data.Proxy
import Reflex
import Reflex.Dom
import Servant.API
import Servant.Reflex

import MockLoginPage.API

main = mainWidgetWithHead htmlHead body

htmlHead :: forall t m. DomBuilder t m => m ()
htmlHead = do
  elAttr "meta" ("charset" =: "utf-8") (pure ())
  elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1.0") (pure ())
  el "title" (text "ui-mockup")
  styleSheet "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
  styleSheet "http://code.ionicframework.com/ionicons/2.0.1/css/ionicons.min.css"
  styleSheet "http://localhost:8081/assets/css/Login-Form-Clean.css"
  where
    styleSheet addr = elAttr "link" ("rel"  =: "stylesheet" <> "href" =: addr) (return ())

api :: Proxy MockApi
api = Proxy

body :: forall t m. MonadWidget t m => m ()
body = do
  -- Instructions to use the server at localhost and to invoke the api
  let url = BaseFullUrl Http "localhost" 8081 ""
      (invokeAPI :<|> _ :<|> _) = client (Proxy @MockApi) (Proxy @m) (constDyn url)

  -- A description of the visual elements
  divClass "login-clean" $ do
    el "form" $ do
      hiddenTitle
      icon
      mail <- _textInput_value <$> mailInputElement
      pass <- _textInput_value <$> passInputElement
      let userResult = liftA2 (User) mail pass
      send <- buttonElement
      forgot

      -- The actual API call
      apiResponse <- invokeAPI (Right <$> userResult) send

      -- A visual feedback on authentication
      r <- holdDyn "Waiting for authentication" $ fmap parseR apiResponse
      el "h2" (dynText r)

--------------------------------------------------------------------------------
-- Implementation of the visual elements:

hiddenTitle, icon :: DomBuilder t m => m ()
hiddenTitle = elClass "h2" "sr-only" (text "Login Form")
icon = divClass "illustration" (elClass "i" "icon ion-ios-navigate" $ pure ())

mailInputElement, passInputElement :: MonadWidget t m => m (TextInput t)
mailInputElement = textInput . configWith $
    ("class" =: "form-control" <> "type" =: "email"
  <> "name" =: "email" <> "placeholder" =: "Email")
passInputElement = textInput . configWith $
    ("class" =: "form-control" <> "type" =: "password"
  <> "name" =: "password" <> "placeholder" =: "Password")
configWith attr = def { _textInputConfig_attributes = constDyn attr }

buttonElement :: DomBuilder t m => m (Event t ())
buttonElement = divClass "form-group" (styledButton "Log in")
  where
    styledButton :: DomBuilder t m => Text -> m (Event t ())
    styledButton t = do
      (e, _) <- element "button" conf (text t)
      return (domEvent Click e)
    conf = def {_elementConfig_initialAttributes =
                   ("class" =: "btn btn-primary btn-block"
                 <> "type" =: "button")}

forgot :: DomBuilder t m => m ()
forgot = elAttr "a"
  ("href" =: "#" <> "class" =: "forgot")
  (text "Forgot your email or password?")

--------------------------------------------------------------------------------
-- Parse the response from the API
parseR :: ReqResult Text -> Text
parseR (ResponseSuccess a b) = a
parseR (ResponseFailure a b) = "ResponseFailure: " <> a
parseR (RequestFailure s) = "RequestFailure: " <> s
