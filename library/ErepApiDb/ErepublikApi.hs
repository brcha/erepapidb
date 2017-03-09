{-# OPTIONS -XOverloadedStrings #-}

-- ----- BEGIN LICENSE BLOCK -----
-- Version: MPL 2.0
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.
--
-- ----- END LICENSE BLOCK -----

--
-- Copyright (c) 2017, Filip Brcic <brcha@gna.org>. All rights reserved.
--
-- This file is part of ErepApiDb
--

module ErepApiDb.ErepublikApi (
    hash_hmac_sha256
  , api_call
) where

import Crypto.MAC.HMAC
import Crypto.Hash hiding (Context)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC8
import qualified Data.ByteString.Lazy as BL
import Data.Char

import Web.FormUrlEncoded

import Network.Curl

import Data.Time.Clock
import Data.Time.Format

hash_hmac_sha256 :: B.ByteString -> B.ByteString -> HMAC SHA256
hash_hmac_sha256 = hmac

-- Hardcoded private/public keys might be a bad idea
api_call :: (String, String) -> String -> String -> [(String, Int)] -> IO (CurlCode, String)
api_call (public, private) resource action query = do
      currentTime <- getCurrentTime
      let
        timeString = formatTime defaultTimeLocale rfc1123DateFormat currentTime
        messageString = BC8.concat [ (BC8.map toLower $ BC8.pack $ resource ++ ":" ++ action ++ digestAddon ++ ":"), (BC8.pack timeString) ]
        digest = hash_hmac_sha256 (BC8.pack private) messageString
        headers = [("Date: " ++ timeString),
                   ("Auth: " ++ public ++ "/" ++ (show $ hmacGetDigest digest))]
        curlOptions = method_GET ++ [ (CurlHttpHeaders headers) ]
      curlGetString urlString curlOptions
    where
      urlString = "http://api.erepublik.com/" ++ resource ++ "/" ++ action ++ queryAddon
      queryAddon = if length query > 0 then "?" ++ urlEncodedQuery else ""
      digestAddon = if length query > 0 then ":" ++ urlEncodedQuery else ""
      urlEncodedQuery = BC8.unpack $ BL.toStrict $ urlEncodeAsForm query
      rfc1123DateFormat = "%a, %0d %b %Y %H:%M:%S %z"
