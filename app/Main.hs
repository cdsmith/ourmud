module Main where

import Data.Aeson
import qualified Data.ByteString.Lazy as ByteString
import Data.Functor.Identity
import Model
import Sample

writeWorld :: FilePath -> World Identity -> IO ()
writeWorld path = ByteString.writeFile path . encode

readWorld :: FilePath -> IO (Maybe (World Identity))
readWorld path = decode <$> ByteString.readFile path

main :: IO ()
main = do
  print sampleWorld
  putStrLn "Writing to sample.json..."
  writeWorld "sample.json" sampleWorld
  putStrLn "Reading back from sample.json..."
  Just world <- readWorld "sample.json"
  print world
  putStrLn "Writing to sample1.json..."
  writeWorld "sample1.json" world
