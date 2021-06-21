{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

{-@ LIQUID "--compile-spec" @-}

module Server
    ( runServer
    , runTask
    , initDB
    , ServerOpts(..)
    , Stage(..)
    )
where

import           Control.Monad                  ( when )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Control.Monad.Reader           ( MonadReader(..)
                                                , ReaderT(..)
                                                )
import           Database.Persist.Sqlite        ( SqlBackend
                                                , runSqlite
                                                , runMigration
                                                , createSqlitePool
                                                )
import           System.FilePath               as P
import           System.Directory
import           System.Environment
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as LBS
import           Network.Mime
import           Frankie.Config
import           Frankie.Auth
import           Data.Maybe
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T

import           Data.Pool                      ( Pool )
import qualified Data.Pool                     as Pool
import           Control.Monad.Base             ( MonadBase(..) )
import           Control.Monad.Trans.Control    ( MonadBaseControl(..) )
import           Control.Monad.Trans.Class      ( lift )
import           Control.Monad.Logger          ( runNoLoggingT )

import           Data.Typeable
import           Data.Data
import           Storm.Core
import           Storm.Frankie
import           Storm.Infrastructure
import           Storm.JSON

import           Storm.SMTP             -- TODO: DUMMY RECURSIVE IMPORTS for LH 
import           Storm.Updates          -- TODO: DUMMY RECURSIVE IMPORTS for LH 

import           Controllers.User
import           Controllers.Auth
import           Controllers.Static 
import           Model
import           Control
import           Routes
import Config    (readAppConfig)

data Stage = Prod | Dev deriving (Data, Typeable, Show)

data ServerOpts = ServerOpts
  { optsPort   :: Port
  , optsHost   :: HostPreference
  , optsStatic :: Maybe String
  , optsPool   :: Int
  , optsDBPath :: T.Text
  }

runServer :: ServerOpts -> IO ()
runServer ServerOpts {..} = runNoLoggingT $ do
    liftIO $ initDB optsDBPath
    cfg  <- liftIO readConfig
    pool <- createSqlitePool optsDBPath optsPool
    liftIO . runFrankieServer "dev" $ do
        mode "dev" $ do
            host optsHost
            port optsPort
            initWithT $ initFromPool cfg pool
        dispatch $ do
            sequence_ routes 
            staticRoute optsStatic
            -- post "/api/signin"         signIn
            -- post "/api/signout"        signOut

-- Routes.hs

-- ? Controller.hs
runTask :: T.Text -> Task a -> IO a
runTask dbpath task = runSqlite dbpath $ do
    cfg     <- liftIO readConfig
    backend <- ask
    liftIO . runTIO $ configure cfg (runReaderT (unTag task) backend)

-- ? Controller.hs
initDB :: T.Text -> IO ()
initDB dbpath = runSqlite dbpath $ do
    runMigration migrateAll

readConfig :: IO Config
readConfig = Config authMethod
                <$> readSecretKey
                <*> readAppConfig

readSecretKey :: IO BS.ByteString
readSecretKey = do
    secret <- fromMaybe "sb8NHmF@_-nsf*ymt!wJ3.KXmTDPsNoy" <$> lookupEnv "VOLTRON_SECRET_KEY"
    return $ T.encodeUtf8 . T.pack $ secret

-- TODO find a way to provide this without exposing the instance of MonadBaseControl
initFromPool :: Config
             -> Pool SqlBackend
             -> Controller ()
             -> TaggedT (Entity User) (ControllerT TIO) ()
initFromPool cfg pool = mapTaggedT run
    where run act = Pool.withResource pool $ configure cfg . runReaderT act

instance MonadBase IO TIO where
    liftBase = TIO

instance MonadBaseControl IO TIO where
    type StM TIO a = a
    liftBaseWith f = TIO (f runTIO)
    restoreM = return

instance MonadBase IO (ControllerT TIO) where
    liftBase = lift . liftBase

instance MonadBaseControl IO (ControllerT TIO) where
    type StM (ControllerT TIO) a = ControllerStatus a
    liftBaseWith f = ControllerT $ \r -> TIO $ fmap Working (f (runTIO . flip runController r))
    restoreM st = ControllerT $ \_ -> return st
