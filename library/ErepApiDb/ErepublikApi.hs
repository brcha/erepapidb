{-# OPTIONS -XOverloadedStrings #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

-- |
-- Module      :  ErepApiDb.ErepublikApi
-- Copyright   :  (c) 2017, Filip Brcic
-- License     :  MPL-2.0
-- Maintainer  :  Filip Brcic <brcha@gna.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- The eRepublik 'api_call' function.

module ErepApiDb.ErepublikApi (
    api_call
) where

import           Crypto.Hash           hiding (Context)
import           Crypto.MAC.HMAC
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC8
import qualified Data.ByteString.Lazy  as BL
import           Data.Char

import           Web.FormUrlEncoded

import           Network.Curl

import           Data.Time.Clock
import           Data.Time.Format

import           Data.Monoid

-- | 'hash_hmac_sha256' returns hmac hash using SHA256
hash_hmac_sha256 :: B.ByteString -> B.ByteString -> HMAC SHA256
hash_hmac_sha256 = hmac

-- | 'api_call' is the main function used to interact with api.erepublik.com server
api_call :: (String, String)      -- ^ Public and private keys for eRepublik's API
         -> String                -- ^ Resource that is being accessed
         -> String                -- ^ Action on the resource
         -> [(String, Int)]       -- ^ List of parameters for the action (or empty list)
         -> IO (CurlCode, String) -- ^ Returns Curl's exit code and the returned yaml code
api_call (public, private) resource action query = do
      currentTime <- getCurrentTime
      let
        timeString = formatTime defaultTimeLocale rfc1123DateFormat currentTime
        resourceUrlPart = mconcat [ resource, ":", action, digestAddon, ":" ]
        messageString = BC8.concat [ ( BC8.map toLower . BC8.pack $ resourceUrlPart ), (BC8.pack timeString) ]
        digest = hash_hmac_sha256 (BC8.pack private) messageString
        headers = [("Date: " <> timeString),
                   ("Auth: " <> public <> "/" <> (show $ hmacGetDigest digest) ) ]
        curlOptions = method_GET <> [ (CurlHttpHeaders headers) ]
      curlGetString urlString curlOptions
    where
      urlString = "http://api.erepublik.com/" <> resource <> "/" <> action <> queryAddon
      queryAddon = if not (null query) then "?" <> urlEncodedQuery else ""
      digestAddon = if not (null query) then ":" <> urlEncodedQuery else ""
      urlEncodedQuery = BC8.unpack . BL.toStrict $ urlEncodeAsForm query
      rfc1123DateFormat = "%a, %0d %b %Y %H:%M:%S %z"
