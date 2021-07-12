{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

{-@ LIQUID "--no-pattern-inline" @-}

module Controllers.List where

import           Storm.Actions      -- LH: name resolution 
import           Storm.Frankie (requireAuthUser,  status200 )
import           Storm.SMTP        -- LH: name resolution bug 
import           Control
import           Model             -- LH: name resolution bug
import           Storm.JSON (respondJSON)
import           Storm.Time ()
import qualified Data.Text as T
import           Storm.Infrastructure
import           Control.Monad.Time (MonadTime(currentTime))
import           Util (tShow)

------------------------------------------------------------------------------
-- | template "ping-pong" respond
------------------------------------------------------------------------------
pong :: Controller ()
pong = respondJSON status200 ("pong" :: T.Text)
