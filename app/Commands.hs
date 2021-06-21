-- | This module has the various CLI options for starting the server

{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Commands where

import           System.Console.CmdArgs
import           Frankie                        ( Port ) 
import qualified Data.Text                     as T

appModes :: Command
appModes = modes 
  [ modeServer    &= auto
  , modeAddUser   &= explicit &= name "add-user"
  ]

data Command
  = Server 
    { port   :: Port
    , host   :: String
    , static :: Maybe String
    , pool   :: Int
    , db     :: T.Text
    }
  | AddUser
    { email     :: T.Text
    , password  :: T.Text
    , firstName :: T.Text
    , lastName  :: T.Text
    , db        :: T.Text
    }
    deriving (Data, Typeable, Show)

modeServer :: Command
modeServer = Server
  { port   = 3000 &= typ "PORT" 
                  &= help "The port to bind to (default 3000)"
  , host   = "127.0.0.1" 
                  &= typ "HOST" 
                  &= help "The interface to bind to (default 127.0.0.1)"
  , pool   = 1    &= typ "SIZE" 
                  &= help "Sql Backend pool size (default 1)"
  , static = def  &= typ "PATH" 
                  &= help "If specified serve any unknown route from this directory"
  , db     = "db.sqlite" 
                  &= typ "PATH" 
                  &= help "Database path (default db.sqlite)"
  }

modeAddUser :: Command
modeAddUser = AddUser
  { email     = "" &= typ "EMAIL"
  , password  = "" &= typ "PASSWORD"
  , firstName = "" &= typ "STRING"
  , lastName  = "" &= typ "STRING"
  , db        = "db.sqlite" 
                  &= typ "PATH" 
                  &= help "Database path (default db.sqlite)"
  }