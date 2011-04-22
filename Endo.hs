module Endo (Base(I, C, F, P), toDna, DNA, RNA) where

import qualified Data.Sequence as Seq

data Base = I | C | F | P
  deriving (Eq, Show, Read)
type DNA = Seq.Seq Base
type RNA = Seq.Seq Base

charToBase 'I' = I
charToBase 'C' = C
charToBase 'F' = F
charToBase 'P' = P

toDna :: String -> DNA
toDna a = Seq.fromList $ map charToBase a


