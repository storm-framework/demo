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

------------------------------------------------------------------------------
-- | Extract User Info List
------------------------------------------------------------------------------
{-@ list :: UserId -> TaggedT<{\_ -> False}, {\_ -> True}> _ _ _ @-}
list :: UserId -> Controller ()
list uid = do
  time   <- getTime
  respondJSON status200 ("OK: hello " <> tShow uid <> " at " <> time :: T.Text)

getTime :: Controller T.Text
getTime = tShow <$> liftTIO currentTime
