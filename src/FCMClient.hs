{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}


-- | Firebase Cloud Messaging google client.
-- https://firebase.google.com/docs/cloud-messaging/concept-options#notifications_and_data_messages
module FCMClient (
  fcmCallJSON
, fcmJSONRequest
) where


import Control.Exception (handle)
import FCMClient.Types (FCMClientError (..), FCMResult (..))
import Network.HTTP.Client (HttpException, Request (..), RequestBody (..), Response (..),
                            parseRequest_)
import Network.HTTP.Simple (httpLBS)
import Network.HTTP.Types (hAuthorization, hContentType, status200, status400, status401,
                           statusIsServerError)

import qualified Data.Aeson as J
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as T


-- | Makes an FCM JSON request, expects a JSON response.
--   https://firebase.google.com/docs/cloud-messaging/http-server-ref#send-downstream
fcmCallJSON :: (J.ToJSON req)
            => String -- ^ firebase project id
            -> B.ByteString -- ^ access token
            -> req -- ^ FCM JSON message, a typed model or a document object, see 'FCMClient.Types'
            -> IO FCMResult
fcmCallJSON firebaseProjectId accessToken fcmMessage =
  handle (\ (he :: HttpException) -> return $ FCMResultError . FCMClientHTTPError . T.pack . show $ he) $ do
    hRes <- httpLBS (fcmJSONRequest firebaseProjectId accessToken (J.encode fcmMessage))
    return $ decodeRes (responseBody hRes) (responseStatus hRes)

  where decodeRes rb rs | rs == status200 = case J.eitherDecode' rb
                                              of Left e  -> FCMResultError $ FCMClientJSONError (T.pack e)
                                                 Right b -> FCMResultSuccess b
                        | rs == status400 = FCMResultError $ FCMErrorResponseInvalidJSON (textBody rb)
                        | rs == status401 = FCMResultError FCMErrorResponseInvalidAuth
                        | statusIsServerError rs = FCMResultError $ FCMServerError rs (textBody rb)
                        | otherwise              = FCMResultError $ FCMClientHTTPError $ "Unexpected response [" <> (T.pack . show) rs <> "]: " <> textBody rb

        textBody = T.decodeUtf8 . L.toStrict


-- | Constructs an authenticated FCM JSON request, body and additional parameters such as
--   proxy or http manager can be set for a customized HTTP call.
fcmJSONRequest :: String -- firebase project id
               -> B.ByteString -- ^ access token
               -> L.ByteString -- ^ JSON POST data
               -> Request
fcmJSONRequest firebaseProjectId accessToken jsonBytes =
  (parseRequest_ $ "https://fcm.googleapis.com/v1/projects/" <> firebaseProjectId <> "/messages:send")
    { method = "POST"
    , requestHeaders = [ (hAuthorization, "Bearer " <> accessToken)
                       , (hContentType, "application/json")
                       ]
    , requestBody = RequestBodyLBS jsonBytes
    }
