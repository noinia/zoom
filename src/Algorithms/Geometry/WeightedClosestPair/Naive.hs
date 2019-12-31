module Algorithms.Geometry.WeightedClosestPair.Naive( wDist
                                                    , weightedClosestPair
                                                    ) where

import Control.Lens
import Data.Ext
import Data.Geometry.Ball
import Data.Geometry.Point
import Data.UnBounded
import Data.Util
import Data.Ord(comparing)
import Data.List(minimumBy)


-- | weighted distance between two disks p and q, i.e. the scaling t
-- such that p*t and q*t touch in a point.
wDist                         :: Floating r => Disk p r -> Disk p r -> r
wDist (Disk p wp) (Disk q wq) = let d = euclideanDist (p^.core) (q^.core)
                                    w = sqrt wp + sqrt wq
                                in  d / w

-- | Computes the weighted closest pair distance in a brute force manner.
-- returns Top (i.e. +infty) if we have only one or two disks.
--
-- running time: \(O(n^2)\)
weightedClosestPair :: (Floating r, Ord r) => [Disk p r :+ q] -> Top (r, Two (Disk p r :+ q))
weightedClosestPair = \case
    []  -> Top
    [_] -> Top
    sds -> ValT . minimumOn fst . map (\(SP p q) -> (wDist (p^.core) (q^.core), Two p q))
                . uniquePairs $ sds

minimumOn   :: (Foldable t, Ord a) => (b -> a) -> t b -> b
minimumOn f = minimumBy (comparing f)
