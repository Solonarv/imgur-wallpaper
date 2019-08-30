{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Imgur where

import Control.Lens
import Data.Aeson.Lens
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import FileEmbedLzma
import Network.HTTP.Client
import Network.HTTP.Req

clientID, clientSecret :: ByteString
(clientID, clientSecret) =
    second (ByteString.drop 1)
  $ ByteString.break (== 0x20) -- 0x20 is space
  $ $(embedFile "data/imgur-auth")

clientAuth :: Option Https
clientAuth = header "Authorization" ("Client-ID " <> clientID)

apiEndpoint :: Url Https
apiEndpoint = https "api.imgur.com" /: "3"

data ImageInfo = ImageInfo (Url Https) (Text)

getAlbumImgs :: Text -> Env -> IO [ImageInfo]
getAlbumImgs aid mgr = runReqMgr mgr do
  response <- responseBody <$> req
    GET
    (apiEndpoint /: "album" /~ aid)
    NoReqBody
    jsonResponse
    clientAuth
  let imgDatas = toListOf (key "data" . values) response
      imgInfos = imgDatas <&> liftA2 ImageInfo
        (^?! key "link" . to parseUrlHttps . _Just . _1)
        (view (key "id"))
  pure imgInfos
