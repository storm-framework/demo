{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

{-@ LIQUID "--no-pattern-inline" @-}

module Controllers.List where

import           Control.Monad.Time (MonadTime(currentTime))
import qualified Data.Text as T

import           Storm.Actions      -- LH: name resolution 
import           Storm.Frankie  (requireAuthUser, status200 )
import           Storm.SMTP        -- LH: name resolution bug 
import           Storm.JSON     (respondJSON, notFoundJSON, decodeBody)
import           Storm.Time     ()
import           Storm.Insert   (insert)
import           Storm.Helpers
import           Storm.Infrastructure

import           Control
import           Model             -- LH: name resolution bug
import           Util           (tShow)
import           Types

------------------------------------------------------------------------------
-- | template "ping-pong" respond
------------------------------------------------------------------------------
pong :: Controller ()
pong = respondJSON status200 ("pong" :: T.Text)

