module Dna (execute, toDna, DNA, RNA) where

import Endo
import qualified Data.Sequence as Seq
import qualified Data.Map as Map
import Control.Concurrent.Chan
import Data.Maybe
import System.Exit


data PItem = PItemBase Base | PItemSkip Integer | PItemQuery DNA | PItemLParen | PItemRParen | PItemRNA RNA
  deriving (Show)
data TItem = TItemBase Base | TItemRef Integer Integer | TItemLength Integer | TItemFinish | TItemRNA RNA
  deriving (Show)


mapFst f (a, b) = (f a, b)
--mapFstOfJust f v = maybe Nothing (Just (mapFst f)) v
mapFstOfJust f v = maybe Nothing (\(a, r) -> Just (f a, r)) v

nat :: DNA -> Maybe (Integer, DNA)
nat dna = case Seq.viewl dna of
            P Seq.:< rest -> Just (0, rest)
            I Seq.:< rest -> combine rest (* 2)
            F Seq.:< rest -> combine rest (* 2)
            C Seq.:< rest -> combine rest (\n -> 2 * n + 1) 
            otherwise     -> Nothing
          where combine rest f = mapFstOfJust f (nat rest)

consts :: DNA -> (DNA, DNA)
consts dna = consts' dna Seq.empty
 where consts' dna const = case Seq.viewl dna of
            C Seq.:< rest -> consts' rest (I Seq.<| const)
            F Seq.:< rest -> consts' rest (C Seq.<| const)
            P Seq.:< rest -> consts' rest (F Seq.<| const)
            I Seq.:< rest -> case Seq.viewl rest of
                               C Seq.:< rest' -> consts' rest' (P Seq.<| const)
                               otherwise      -> (const, rest)
            otherwise     -> (const, dna)


pitem :: DNA -> Maybe (PItem, DNA)
pitem dna = case Seq.viewl dna of
          C Seq.:< rest -> Just (PItemBase I, rest)
          F Seq.:< rest -> Just (PItemBase C, rest)
          P Seq.:< rest -> Just (PItemBase F, rest)
          I Seq.:< rest -> case Seq.viewl rest of
                             C Seq.:< rest' -> Just (PItemBase P, rest')
                             P Seq.:< rest' -> mapFstOfJust PItemSkip (nat rest')
                             F Seq.:< rest' -> Just . mapFst PItemQuery $ consts (Seq.drop 1 rest')
                             I Seq.:< rest' -> case Seq.viewl rest' of
                                                 P Seq.:< rest'' -> Just (PItemLParen, rest'')
                                                 C Seq.:< rest'' -> Just (PItemRParen, rest'')
                                                 F Seq.:< rest'' -> Just (PItemRParen, rest'')
                                                 I Seq.:< rest'' -> Just $ mapFst PItemRNA (Seq.splitAt 7 rest'')
                                                 otherwise       -> Nothing
                             otherwise -> Nothing
          otherwise -> Nothing

pattern :: DNA -> ([RNA], Maybe ([PItem], DNA))
pattern = pattern' [] [] 0
  where pattern' rna pat lvl dna = case pitem dna of
                                    Just (PItemLParen, rest) -> pattern' rna (PItemLParen:pat) (lvl+1) rest
                                    Just (PItemRParen, rest) -> if lvl > 0 then pattern' rna (PItemRParen:pat) (lvl-1) rest else (reverse rna, Just (reverse pat, rest))
                                    Just (PItemRNA r, rest)  -> pattern' (r:rna) pat lvl rest
                                    Nothing                  -> (reverse rna, Nothing)
                                    Just (item, rest)        -> pattern' rna (item:pat) lvl rest


titem :: DNA -> Maybe (TItem, DNA)
titem dna = case Seq.viewl dna of
          C Seq.:< rest -> Just (TItemBase I, rest)
          F Seq.:< rest -> Just (TItemBase C, rest)
          P Seq.:< rest -> Just (TItemBase F, rest)
          I Seq.:< rest -> case Seq.viewl rest of
                             C Seq.:< rest' -> Just (TItemBase P, rest')
                             F Seq.:< rest' -> titem_ref rest'
                             P Seq.:< rest' -> titem_ref rest'
                             I Seq.:< rest' -> case Seq.viewl rest' of
                                                 P Seq.:< rest'' -> mapFstOfJust TItemLength (nat rest'')
                                                 C Seq.:< rest'' -> Just (TItemFinish, rest'')
                                                 F Seq.:< rest'' -> Just (TItemFinish, rest'')
                                                 I Seq.:< rest'' -> Just $ mapFst TItemRNA (Seq.splitAt 7 rest'')
                                                 otherwise       -> Nothing
                             otherwise      -> Nothing
          otherwise     -> Nothing

  where titem_ref dna = do (n, r) <- nat dna
                           (n2, r') <- nat r
                           return $ (TItemRef n n2, r')

template :: DNA -> ([RNA], Maybe ([TItem], DNA))
template = template' [] []
  where template' rna tem dna = case titem dna of
                                  Just (TItemFinish, dna')  -> (reverse rna, Just (reverse tem, dna'))
                                  Just (TItemRNA r, dna')   -> template' (r:rna) tem dna'
                                  Just (item, dna')         -> template' rna (item:tem) dna'
                                  Nothing                   -> (reverse rna, Nothing)

match :: DNA -> [PItem] -> Maybe ([DNA], DNA)
match dna = match' [] 0 [] dna
  where match' ixs i env dna' ((PItemBase base):pat) = case Seq.viewl dna' of
                                                         base Seq.:< rest -> match' ixs (i+1) env rest pat
                                                         otherwise        -> Nothing
        match' ixs i env dna' ((PItemSkip n):pat)    = if (fromInteger $ i + n) <= (Seq.length dna) 
                                                       then match' ixs (i + n) env (Seq.drop (fromInteger n) dna') pat
                                                       else Nothing
        match' ixs i env dna' ((PItemQuery s):pat)   = case indexOfEnd s dna' of
                                                         Just n  -> match' ixs ((fromIntegral n) + i) env (Seq.drop n dna') pat
                                                         Nothing -> Nothing
        match' ixs i env dna' (PItemLParen:pat)      = match' (i:ixs) i env dna' pat
        match' (c0:ixs) i env dna' (PItemRParen:pat) = match' ixs i ((Seq.take (fromInteger $ i - c0) $ Seq.drop (fromInteger c0) dna):env) dna' pat
        match' ixs i env dna' []                     = Just (env, dna')

indexOfEnd q dna = maybe Nothing (\x -> Just (x + q_len)) (indexOfStart q dna (dna_len - q_len) 0)
  where q_len = Seq.length q
        dna_len = Seq.length dna
        indexOfStart q dna p i
          | p < 0      = Nothing
          | otherwise  = if (Seq.take q_len dna) == q then Just i else indexOfStart q (Seq.drop 1 dna) (p - 1) (i + 1)

protect 0 d = d
protect l d = protect (l-1) $ quote d

quote d = quote' d Seq.empty
  where quote' d quoted = case Seq.viewl d of
                            I Seq.:< rest -> quote' rest (quoted Seq.|> C)
                            C Seq.:< rest -> quote' rest (quoted Seq.|> F)
                            F Seq.:< rest -> quote' rest (quoted Seq.|> P)
                            P Seq.:< rest -> quote' rest (quoted Seq.|> I Seq.|> C)
                            otherwise     -> quoted

asnat 0 = Seq.singleton P
asnat n
  | n `mod` 2 == 0 = I Seq.<| (asnat $ n `div` 2)
  | otherwise      = C Seq.<| (asnat $ n `div` 2)

replace :: DNA -> [TItem] -> [DNA] -> DNA
replace dna titems env = foldr (Seq.><) dna (map applyTitem titems)
  where lookupEnv n = head (drop (fromInteger n) env)
        applyTitem (TItemBase b)  = Seq.singleton b
        applyTitem (TItemRef l n) = protect l $ lookupEnv n
        applyTitem (TItemLength n) = asnat $ Seq.length $ lookupEnv n

stopIfFailed :: Maybe a -> IO a
stopIfFailed Nothing  = do exitWith ExitSuccess
stopIfFailed (Just x) = do return x

printRnaAndStopIfFailed :: Chan RNA -> ([RNA], Maybe a) -> IO a
printRnaAndStopIfFailed chan (rna, r) = do writeList2Chan chan rna
                                           stopIfFailed r

executeIteration :: Chan RNA -> DNA -> IO DNA
executeIteration rnapipe dna = do (p, rest) <- printRnaAndStopIfFailed rnapipe $ pattern dna
                                  (t, rest') <- printRnaAndStopIfFailed rnapipe $ template rest
                                  (env, rest'') <- stopIfFailed $ match rest' p
                                  return (replace rest'' t env)

sample1data = Seq.fromList [I,I,P,I,P,I,C,P,I,I,C,I,C,I,I,F,I,C,C,I,F,P,P,I,I,C,C,F,P,C] 
sample2data = Seq.fromList [I,I,P,I,P,I,C,P,I,I,C,I,C,I,I,F,I,C,C,I,F,C,C,C,P,P,I,I,C,C,F,P,C]
sample3data = Seq.fromList [I,I,P,I,P,I,I,C,P,I,I,C,I,I,C,C,I,I,C,F,C,F,C]
-- sample1 = executeIteration $ sample1data -- should be PICFC
-- sample2 = executeIteration $ sample2data -- should be PIICFCFFPC
-- sample3 = executeIteration $ sample3data -- should be I

execute :: Chan RNA -> DNA -> IO ()
execute rnapipe dna = do dna' <- executeIteration rnapipe dna
                         execute rnapipe dna'


