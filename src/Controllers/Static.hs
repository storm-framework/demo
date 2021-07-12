-- | This module has some controllers useful for 'static' routes, e.g. to serve up files
{-@ LIQUID "--no-pattern-inline" @-}

module Controllers.Static where
import qualified Data.Text                     as T
import qualified Data.ByteString.Lazy          as LBS
import           System.FilePath ((</>), joinPath)
import           System.Directory (doesFileExist)
import           Network.Mime (defaultMimeLookup)

import           Storm.Infrastructure
import           Storm.Frankie
import           Storm.SMTP             -- TODO: DUMMY RECURSIVE IMPORTS for LH 
import           Storm.Updates          -- TODO: DUMMY RECURSIVE IMPORTS for LH 
import Model                            -- TODO: DUMMY RECURSIVE IMPORTS for LH 

import           Control (Controller)

-- | Respond with a particular (static) File

sendFromDirectory :: FilePath -> FilePath -> Controller ()
sendFromDirectory dir fallback = do
    req <- request
    let path = dir </> joinPath (map T.unpack (reqPathInfo req))
    exists <- liftTIO . TIO $ doesFileExist path
    if exists then sendFile path else sendFile (dir </> fallback)

-- ? Controllers/Static.hs files
sendFile :: FilePath -> Controller ()
sendFile path = do
    let mime = defaultMimeLookup (T.pack path)
    content <- liftTIO . TIO . LBS.readFile $ path
    respondTagged $ Response status200 [(hContentType, mime)] content

