{-# OPTIONS -XOverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

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

-- | An example module.
module ErepApiDb (
    main
  , module ErepApiDb.ErepublikApi
) where

import ErepApiDb.ErepublikApi

import Control.Monad
import Env

data ErepApiDbOptions = ErepApiDbOptions { publicKey :: String, privateKey :: String}

erepApiDbOptions :: IO ErepApiDbOptions
erepApiDbOptions = Env.parse (header "ErepApiDb") $
    ErepApiDbOptions <$> var (str <=< nonempty) "EREP_PUBLIC_KEY"  (help "Public key for eRepublik API")
                     <*> var (str <=< nonempty) "EREP_PRIVATE_KEY" (help "Private key for eRepublik API")

-- | An example function.
main :: IO ()
main = do
  ErepApiDbOptions {publicKey, privateKey} <- erepApiDbOptions
  (curlCode, text) <- api_call (publicKey, privateKey) "citizen" "profile" [("citizenId", 1257892)]
  putStrLn text
  return ()
