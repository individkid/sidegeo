module SideGeo.Container where

import qualified Data.Set
import qualified Data.Map
import qualified Numeric.LinearAlgebra.HMatrix as HMatrix

type Set = Data.Set.Set
type Map = Data.Map.Map
type Scalar = Double
type Matrix = HMatrix.Matrix Scalar
type Vector = HMatrix.Vector Scalar

class (Ord a, Enum a) => Holes a where
 zero :: a
