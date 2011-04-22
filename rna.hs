module Rna (drawRna) where

import Endo
import Control.Concurrent.Chan

drawRna :: Chan RNA -> IO ()
drawRna rnapipe = do rna <- readChan rnapipe
                     putStrLn $ show rna
                     drawRna rnapipe
