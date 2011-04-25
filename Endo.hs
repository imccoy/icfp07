module Endo (Base(I, C, F, P), toDna, DNA, RNA, charToBase, baseToChar) where

import qualified Data.Sequence as Seq

data Base = I | C | F | P
  deriving (Eq, Show, Read)
type DNA = Seq.Seq Base
type RNA = Seq.Seq Base

charToBase 'I' = I
charToBase 'C' = C
charToBase 'F' = F
charToBase 'P' = P
charToBase x   = error $ "Not-a-base " ++ (show x)

baseToChar I = 'I'
baseToChar C = 'C'
baseToChar F = 'F'
baseToChar P = 'P'

toDna :: String -> DNA
toDna a = Seq.fromList $ map charToBase a


