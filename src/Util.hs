{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Util
  ( stripPrefix
  , module A
  , traceShow
  , tShow
  )
where

import           Data.Aeson
import qualified Data.Aeson                    as A
import           Data.Char
import qualified Debug.Trace
import qualified Data.Text                     as T 


tShow :: (Show a) => a -> T.Text
tShow = T.pack . show

traceShow :: (Show a) => String -> a -> a
traceShow msg x = Debug.Trace.trace (msg <> ": " <> show x) x

stripPrefix :: String -> A.Options
stripPrefix prefix =
  A.defaultOptions { A.fieldLabelModifier = headToLower . stripLongestPrefix prefix }

stripLongestPrefix :: Eq a => [a] -> [a] -> [a]
stripLongestPrefix (x : xs) (y : ys) | x == y = stripLongestPrefix xs ys
stripLongestPrefix _ ys                       = ys


{-@ ignore headToLower @-}
headToLower :: String -> String
headToLower []       = error "field became empty"
headToLower (x : xs) = toLower x : xs
