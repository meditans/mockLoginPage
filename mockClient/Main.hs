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

import MockLoginPage.API

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

-- main = mainWidgetWithHead htmlHead body
main = mainWidgetWithHead htmlHead bodyDressed

api :: Proxy MockApi
api = Proxy

bodyDressed :: forall t m. MonadWidget t m => m ()
bodyDressed = do
  let url = BaseFullUrl Http "localhost" 8081 ""
      (invokeAPI :<|> _ :<|> _) = client (Proxy @MockApi) (Proxy @m) (constDyn url)
  divClass "login-clean" $ do
    divClass "form" $ do
      elClass "h2" "sr-only" (text "Login Form")
      divClass "illustration" $ elClass "i" "icon ion-ios-navigate" (pure ())
      mailInput <- _textInput_value <$> textInput (configWith mailAttributes)
      passInput <- _textInput_value <$> textInput (configWith passAttributes)
      let userResult = liftA2 (User) mailInput passInput
      button <- divClass "form-group" $ myButton "Log in"
      elAttr "a" ("href" =: "#" <> "class" =: "forgot") $ text "Forgot your email or password?"
      apiResponse <- invokeAPI (Right <$> userResult) button
      r <- holdDyn "Waiting for authentication" $ fmap parseR apiResponse
      el "h2" (dynText r)

--------- Styling for css attributes:

configWith attr = def { _textInputConfig_attributes = constDyn attr }

mailAttributes = ("class" =: "form-control"
               <> "type" =: "email"
               <> "name" =: "email"
               <> "placeholder" =: "Email")

passAttributes = ("class" =: "form-control"
               <> "type" =: "password"
               <> "name" =: "password"
               <> "placeholder" =: "Password")

myButton :: DomBuilder t m => Text -> m (Event t ())
myButton t = do
  (e, _) <- element "button" buttonConf $ text t
  return $ domEvent Click e
 where
   buttonConf = def {_elementConfig_initialAttributes = ("class" =: "btn btn-primary btn-block")}

parseR :: ReqResult Text -> Text
parseR (ResponseSuccess a b) = a
parseR (ResponseFailure a b) = "ResponseFailure: " <> a
parseR (RequestFailure s) = "RequestFailure: " <> s
