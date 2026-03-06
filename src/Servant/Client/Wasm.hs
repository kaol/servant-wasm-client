-- | This module provides 'client' which can automatically generate
-- querying functions for each endpoint just from the type representing your
-- API.
module Servant.Client.Wasm
  (
    client
  , ClientM
  , runClientM

    -- * Configuration
  , ClientEnv(..)
  , mkClientEnv

  , module Servant.Client.Core.Reexport
  ) where

import Servant.Client.Internal.WasmFetchClient
import Servant.Client.Core.Reexport
