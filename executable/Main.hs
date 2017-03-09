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

-- It is generally a good idea to keep all your business logic in your library
-- and only use it in the executable. Doing so allows others to use what you
-- wrote in their libraries.
import qualified ErepApiDb

main :: IO ()
main = ErepApiDb.main
