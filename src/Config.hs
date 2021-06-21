-- | This module defines the App-level 'AppConfig' type which is is then 
--   viewable inside `ControllerT` actions via a ReaderT API.
--
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

{-@ LIQUID "--no-pattern-inline" @-}

module Config where

import           Data.Maybe (fromMaybe)
import           System.Environment (lookupEnv)
import           Network.Socket                 ( HostName )
import qualified Control.Concurrent.MVar       as MVar
import qualified Text.Mustache.Types           as Mustache
import qualified Storm.SMTP                    as SMTP
import           Model   -- LH name resolution

---------------------------------------------------------------------------------------------
-- | App specific (read-only) configuration parameters 
---------------------------------------------------------------------------------------------

-- You can add extra configuration stuff here, e.g. SMTP Server configuration ---------------
data AppConfig = AppConfig 
  { configTemplateCache :: !(MVar.MVar Mustache.TemplateCache)
  , configSMTP          :: SMTPConfig
  }

data SMTPConfig = SMTPConfig
  { smtpHost :: HostName
  , smtpUser :: SMTP.UserName
  , smtpPass :: SMTP.Password
  }

readAppConfig :: IO AppConfig
readAppConfig = AppConfig 
                <$> MVar.newMVar mempty
                <*> readSMTPConfig

readSMTPConfig :: IO SMTPConfig
readSMTPConfig = do
    host <- fromMaybe "localhost" <$> lookupEnv "VOLTRON_SMTP_HOST"
    user <- fromMaybe ""          <$> lookupEnv "VOLTRON_SMTP_USER"
    pass <- fromMaybe ""          <$> lookupEnv "VOLTRON_SMTP_PASS"
    return $ SMTPConfig host user pass
