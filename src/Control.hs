-- | This module defines the App-level 'Config' type and 
--   and uses it to define the 'Controller' monad.

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

{-@ LIQUID "--no-pattern-inline" @-}

module Control where

import           Data.ByteString                ( ByteString )
import qualified Data.Text                     as T
import           Control.Monad.Reader           ( ReaderT(..) )
import           Database.Persist.Sqlite        ( SqlBackend )
import qualified Control.Concurrent.MVar       as MVar
import qualified Text.Mustache.Types           as Mustache
import           Network.Socket                 ( HostName )

import           Frankie.Auth ( HasAuthMethod(..), AuthMethod )
import           Frankie.Config ( ConfigT )

import           Storm.Actions     -- LH name resolution
import           Storm.Frankie (FrankieConfigDispatch,  ControllerT )
import           Storm.Core ( Entity )
import           Storm.Infrastructure ( TaggedT, TIO )
import qualified Storm.SMTP                    as SMTP
import           Model   -- LH name resolution
import           Config

---------------------------------------------------------------------------------------------
-- | App specific (read-only) configuration parameters 
---------------------------------------------------------------------------------------------

data Config = Config
  { configAuthMethod    :: !(AuthMethod (Entity User) Controller)
  , configSecretKey     :: ByteString
  , configApp           :: AppConfig
  }

---------------------------------------------------------------------------------------------
-- | The Controller Monad -------------------------------------------------------------------
---------------------------------------------------------------------------------------------

type TasCon m   = TaggedT (Entity User) (ReaderT SqlBackend (ConfigT Config m))
type Controller = TaggedT (Entity User) (ReaderT SqlBackend (ConfigT Config (ControllerT TIO)))
type Task       = TaggedT (Entity User) (ReaderT SqlBackend (ConfigT Config TIO))
type Route      = FrankieConfigDispatch TIO Controller ()

instance Frankie.Auth.HasAuthMethod (Entity User) Controller Config where
  getAuthMethod = configAuthMethod