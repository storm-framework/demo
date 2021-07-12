{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

{-@ LIQUID "--no-pattern-inline" @-}

module Controllers.User where

import qualified Data.Maybe                    as Mb
import qualified Data.Text.Encoding            as T

import           Storm.Core
import           Storm.Actions
import           Storm.Insert
import           Storm.Infrastructure
import           Storm.Frankie
import           Storm.SMTP        -- LH: name resolution bug 
import qualified Frankie.Log                   as Log
import           Control
import           Model
import           Types

import Storm.Crypto

------------------------------------------------------------------------------
-- | Extract User Info List
------------------------------------------------------------------------------
{-@ extractUserNG :: _ -> TaggedT<{\_ -> True}, {\_ -> False}> _ _ _ @-}
extractUserNG :: Entity User -> Controller UserNG
extractUserNG u = do
  firstName <- project userFirstName' u
  lastName  <- project userLastName' u
  return     $ UserNG firstName lastName

-------------------------------------------------------------------------------
-- | Add a user ---------------------------------------------------------------
-------------------------------------------------------------------------------

{-@ addUser :: _ -> TaggedT<{\_ -> True}, {\_ -> True}> _ _ _ @-}
addUser :: (MonadTIO m) => CreateUser -> TasCon m (Maybe UserId)
addUser r@CreateUser {..} = do
  logT Log.INFO ("addUser: " ++ show r)
  EncryptedPass encrypted <- encryptPassTIO' (Pass (T.encodeUtf8 crUserPassword))
  maybeId <- insertMaybe (mkUser crUserEmail encrypted crUserFirst crUserLast)
  if Mb.isNothing maybeId
    then logT Log.WARNING ("addUser: skipping duplicate user " ++ show r)
    else logT Log.INFO ("addUser: adding new user " ++ show r)
  return maybeId
