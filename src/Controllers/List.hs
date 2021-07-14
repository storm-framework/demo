{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

{-@ LIQUID "--no-pattern-inline" @-}

module Controllers.List where

import           Control.Monad.Time (MonadTime(currentTime))
import qualified Data.Text as T

import           Storm.Actions      -- LH: name resolution
import           Storm.Frankie  (requireAuthUser, status200)
import           Storm.SMTP        -- LH: name resolution bug
import           Storm.JSON     (respondJSON, notFoundJSON, decodeBody)
import           Storm.Time     ()
import           Storm.Insert   (insert)
import           Storm.Helpers
import           Storm.Filters
import           Storm.Infrastructure

import           Control
import           Model             -- LH: name resolution bug
import           Util           (tShow)
import           Types
import           Controllers.User (extractUserNG)

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
list userId = do
  viewId  <- project userId' =<< requireAuthUser
  let pub  = if userId == viewId then trueF else itemLevel' ==. "public"
  items   <- selectList (itemOwner' ==. userId) -- &&: pub)
  itemDs  <- mapT (\i -> 
               ItemData `fmap` project itemDescription' i
                        <*>    project itemLevel'       i
	     ) items
  respondJSON status200 itemDs

------------------------------------------------------------------------------
-- | Add a new item for logged in user
------------------------------------------------------------------------------
{-@ add :: TaggedT<{\_ -> False}, {\_ -> True}> _ _ _ @-}
add :: Controller ()
add = do
  owner   <- requireAuthUser
  ownerId <- project userId' owner
  ownerEmail <- project userEmailAddress' owner
  items   <- decodeBody
  mapT (\ItemData {..} -> insert (mkItem ownerId itemDescription itemLevel)) items
  respondJSON status200 ("OK: added " <> tShow (length items) <> " items for " <> ownerEmail)
