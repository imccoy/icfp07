import Data.Sequence as Seq

data Base = I | C | F | P

data PItem = PItemBase Base | PItemSkip Integer | PItemQuery DNA | PItemLParen | PItemRParen | PItemRNA DNA
data TItem = TItemBase Base | TItemRef Integer Integer | TItemLength Integer | TItemFinish | TItemRNA DNA

type DNA = Seq.Seq Base

mapFst f (a, b) = (f a, b)
--mapFstOfJust f v = maybe Nothing (Just (mapFst f)) v
mapFstOfJust f v = maybe Nothing (\(a, r) -> Just (f a, r)) v

nat :: DNA -> Maybe (Integer, DNA)
nat dna = case viewl dna of
            P Seq.:< rest -> Just (0, rest)
            I Seq.:< rest -> combine rest (* 2)
            F Seq.:< rest -> combine rest (* 2)
            C Seq.:< rest -> combine rest (\n -> 2 * n + 1) 
            otherwise     -> Nothing
          where combine rest f = mapFstOfJust f (nat rest)

consts :: DNA -> (DNA, DNA)
consts dna = consts' dna Seq.empty
 where consts' dna const = case viewl dna of
            C Seq.:< rest -> consts' rest (I <| const)
            F Seq.:< rest -> consts' rest (C <| const)
            P Seq.:< rest -> consts' rest (F <| const)
            I Seq.:< rest -> case viewl rest of
                               C Seq.:< rest' -> consts' rest' (P <| const)
                               otherwise      -> (const, rest)
            otherwise     -> (const, dna)


pitem :: DNA -> Maybe (PItem, DNA)
pitem dna = case viewl dna of
          C Seq.:< rest -> Just (PItemBase I, rest)
          F Seq.:< rest -> Just (PItemBase C, rest)
          P Seq.:< rest -> Just (PItemBase F, rest)
          I Seq.:< rest -> case viewl rest of
                             C Seq.:< rest' -> Just (PItemBase P, rest')
                             F Seq.:< rest' -> mapFstOfJust PItemSkip (nat rest')
                             P Seq.:< rest' -> Just . mapFst PItemQuery $ consts (Seq.drop 1 rest')
                             I Seq.:< rest' -> case viewl rest' of
                                                 P Seq.:< rest'' -> Just (PItemLParen, rest'')
                                                 C Seq.:< rest'' -> Just (PItemRParen, rest'')
                                                 F Seq.:< rest'' -> Just (PItemRParen, rest'')
                                                 I Seq.:< rest'' -> Just $ mapFst PItemRNA (Seq.splitAt 7 rest'')
                                                 otherwise       -> Nothing

pattern :: DNA -> Maybe ([PItem], DNA)
pattern = pattern' [] 0
  where pattern' pat lvl dna = case pitem dna of
                                Just (PItemLParen, rest) -> pattern' (PItemLParen:pat) (lvl+1) rest
                                Just (PItemRParen, rest) -> if lvl > 0 then pattern' (PItemRParen:pat) (lvl-1) rest else Just (pat, rest)
                                Nothing                  -> Nothing
                                Just (item, rest)        -> pattern' (item:pat) lvl rest


titem :: DNA -> Maybe (TItem, DNA)
titem dna = case viewl dna of
          C Seq.:< rest -> Just (TItemBase I, rest)
          F Seq.:< rest -> Just (TItemBase C, rest)
          P Seq.:< rest -> Just (TItemBase F, rest)
          I Seq.:< rest -> case viewl rest of
                             C Seq.:< rest' -> Just (TItemBase P, rest')
                             F Seq.:< rest' -> titem_ref rest'
                             P Seq.:< rest' -> titem_ref rest'
                             I Seq.:< rest' -> case viewl rest' of
                                                 P Seq.:< rest'' -> mapFstOfJust TItemLength (nat rest'')
                                                 C Seq.:< rest'' -> Just (TItemFinish, rest'')
                                                 F Seq.:< rest'' -> Just (TItemFinish, rest'')
                                                 I Seq.:< rest'' -> Just $ mapFst TItemRNA (Seq.splitAt 7 rest'')
                                                 otherwise       -> Nothing

  where titem_ref dna = do (n, r) <- nat dna
                           (n2, r') <- nat r
                           return $ (TItemRef n n2, r')

template :: DNA -> Maybe ([TItem], DNA)
template = template' []
  where template' tem dna = case titem dna of
                              Just (TItemFinish, dna') -> Just (tem, dna')
                              Just (item, dna')        -> template' (item:tem) dna'
                              Nothing                  -> Nothing
