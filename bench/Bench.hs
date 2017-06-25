module Main where

import Control.Monad
import Criterion.Main
import Data.Aeson as A
import qualified Data.ByteString as B
import Data.Sajson as S
import System.Environment

dataFiles :: [String]
dataFiles =
  [
    "apache_builds.min.json",
    "github_events.min.json",
    "instruments.min.json",
    "mesh.min.json",
    "svg_menu.min.json",
    "twitter.min.json",
    "update-center.min.json"
  ]

parseWithAeson, parseWithSajson :: B.ByteString -> Maybe Value
parseWithAeson = A.decodeStrict'
parseWithSajson = S.decodeStrict

main :: IO ()
main = do
  mdir <- lookupEnv "SAJSON_BENCHMARK_DATA_DIR"
  let dir = maybe "./" (++"/") mdir
  benches <- forM dataFiles $ \f -> do
    let path = dir ++ f
    contents <- B.readFile path
    pure $ bgroup f [ bench "aeson" $ nf parseWithAeson contents, bench "sajson" $ nf parseWithSajson contents ]
  defaultMain benches
