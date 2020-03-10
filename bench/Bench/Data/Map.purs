module Bench.Data.Map where

import Prelude

import Data.List as L
import Data.Map as M
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Performance.Minibench (bench, benchWith)

benchMap :: Effect Unit
benchMap = do
  log "size"
  log "---------------"
  benchSize

  log ""

  log "fromFoldable"
  log "------------"
  benchFromFoldable

  log "filterKeys"
  log "----------"
  benchFilterKeys

  where

  benchSize = do
    let nats = L.range 0 999999
        natPairs = (flip Tuple) unit <$> nats
        singletonMap = M.singleton 0 unit
        smallMap = M.fromFoldable $ L.take 100 natPairs
        midMap = M.fromFoldable $ L.take 10000 natPairs
        bigMap = M.fromFoldable $ natPairs

    log "size: singleton map"
    bench \_ -> M.size singletonMap

    log $ "size: small map (" <> show (M.size smallMap) <> ")"
    bench \_ -> M.size smallMap

    log $ "size: midsize map (" <> show (M.size midMap) <> ")"
    benchWith 100 \_ -> M.size midMap

    log $ "size: big map (" <> show (M.size bigMap) <> ")"
    benchWith 10  \_ -> M.size bigMap

  benchFromFoldable = do
    let natStrs = show <$> L.range 0 99999
        natPairs = (flip Tuple) unit <$> natStrs
        shortPairList = L.take 10000 natPairs
        tinyPairList = L.take 30 natPairs

    log $ "fromFoldable (" <> show (L.length tinyPairList) <> ")"
    bench \_ -> M.fromFoldable tinyPairList

    log $ "fromFoldable (" <> show (L.length shortPairList) <> ")"
    benchWith 100 \_ -> M.fromFoldable shortPairList

    log $ "fromFoldable (" <> show (L.length natPairs) <> ")"
    benchWith 10 \_ -> M.fromFoldable natPairs

  benchFilterKeys = do
    let nats = L.range 0 999999
        natPairs = (flip Tuple) unit <$> nats
        singletonMap = M.singleton 0 unit
        smallMap = M.fromFoldable $ L.take 100 natPairs
        midMap = M.fromFoldable $ L.take 10000 natPairs
        bigMap = M.fromFoldable $ natPairs

        filter = M.filterKeys (\k -> k `mod` 2 == 0)

    log "filterKeys: singleton map"
    bench \_ -> filter singletonMap

    log $ "filterKeys: small map (" <> show (M.size smallMap) <> ")"
    bench \_ -> filter smallMap

    log $ "filterKeys: midsize map (" <> show (M.size midMap) <> ")"
    benchWith 100 \_ -> filter midMap

    log $ "filterKeys: big map (" <> show (M.size bigMap) <> ")"
    benchWith 10 \_ -> filter bigMap
