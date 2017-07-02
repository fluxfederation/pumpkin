{-# LANGUAGE OverloadedStrings #-}

module Auth
  ( secretAuth
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS8
import Network.HTTP.Types.Status
import Network.Wai

secretAuth :: String -> Middleware
secretAuth secret wrapped req respond =
  let pass = wrapped req respond
  in case lookup "Authorization" (requestHeaders req) of
       Nothing ->
         respond (responseLBS status401 [] "Missing Authorization header")
       Just authKey ->
         if (last . B.split 32) authKey == BS8.pack secret
           then pass
           else respond (responseLBS status403 [] "Invalid Authorization")
