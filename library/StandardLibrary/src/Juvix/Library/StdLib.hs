{-# LANGUAGE ViewPatterns #-}
module Juvix.Library.StdLib where

import Protolude
import System.Directory
import Text.Pretty.Simple
import qualified Data.ByteString.Lazy as BL
import Control.Lens
import qualified Network.Wreq as Wreq
import Data.Aeson.Lens (key, _Object)
import qualified Data.HashMap.Strict as Map
import System.FilePath.Posix

juvixAWS, juvixBucket, checksumsFile :: IsString p => p
juvixAWS = "https://heliax-juvix-artifacts-v1.s3.eu-west-1.amazonaws.com"
juvixBucket = "heliax-juvix-artifacts-v1"
checksumsFile = "checksums.json"

createStdLibDir :: FilePath -> IO ()
createStdLibDir p = do
  d <- getJuvixHome "stdlib/"
  createDirectoryIfMissing True (d <> p) 

getJuvixHome :: FilePath -> IO FilePath
getJuvixHome p =  do
  d <- (<> ("/.juvix/" <> p)) <$> getHomeDirectory
  createDirectoryIfMissing True (d <> p) 
  return d

downloadStdLibs :: IO ()
downloadStdLibs = do
  r <- Wreq.asValue =<< (Wreq.get $ juvixAWS ++ "/" ++ checksumsFile)
  let files = r ^.  Wreq.responseBody . _Object
  localJuvix <- getJuvixHome "stdlib/"
  putStrLn $ "args: " ++ show files
  sequence_ $ Map.mapWithKey (\filename checksum -> do
    retrieveFile localJuvix (toS filename)
    ) files
  where
    retrieveFile localJuvix filename = do
      r <- (Wreq.get $ juvixAWS ++ "/" ++ filename)
      let content = r ^.  Wreq.responseBody
      createStdLibDir (takeDirectory filename)
      BL.writeFile (localJuvix <> filename) content

localStdLibs :: IO Bool
localStdLibs = do
  d <- getJuvixHome "stdlib/"
  doesDirectoryExist d

loadStdLibs :: IO ()
loadStdLibs = do
  -- TODO: How long do we want to cache this?
  -- TODO: Checksum
  -- success <- localStdLibs
  downloadStdLibs
