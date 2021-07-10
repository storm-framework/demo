
{-@ LIQUID "--compile-spec" @-}

{-# LANGUAGE OverloadedStrings #-}

module Routes 
    ( -- * Top-level routes
      routes
      -- * Static route
    , staticRoute
    ) where

import qualified Data.Text as T
import           Storm.Frankie
import           Storm.SMTP             -- TODO: DUMMY RECURSIVE IMPORTS for LH 
import           Storm.Updates          -- TODO: DUMMY RECURSIVE IMPORTS for LH 
import           System.FilePath (joinPath)
import           Storm.JSON (respondError)
import           Model
import           Control (Route)
import           Controllers.Auth 
import           Controllers.List
import           Controllers.Static ( sendFromDirectory )

-------------------------------------------------------------------------------
-- | Top-level Routes for the App ---------------------------------------------
-------------------------------------------------------------------------------

routes :: [Route]
routes =
  [ post "/api/signin"         signIn
  , post "/api/signout"        signOut
  , get  "/api/list"           list
  ]

staticRoute :: Maybe FilePath -> Route 
staticRoute optsStatic = 
  case optsStatic of
    Just path -> fallback $ 
      sendFromDirectory path "index.html"
    Nothing   -> fallback $ do
      req <- request
      let path = joinPath (map T.unpack (reqPathInfo req))
      respondError status404 (Just ("Route not found: " ++ path))