{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           GHC.Exts                       ( fromString )
import System.Console.CmdArgs ( cmdArgs )
import Server ( runServer, runTask, ServerOpts(ServerOpts) )
import           Commands
import qualified Controllers.User as User
import Types ( mkCreateUser )

main :: IO ()
main = do
  args <- cmdArgs appModes
  case args of

    Server {..} -> do
      putStrLn $ "Starting server at: host = " ++ show (fromString host) ++ ", port = " ++ show port
      runServer $ ServerOpts port (fromString host) static pool db

    AddUser {..} -> do
      let thing = mkCreateUser email password firstName lastName
      rId <- runTask db $ User.addUser thing
      putStrLn ("Add User: " ++ show rId)
