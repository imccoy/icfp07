import Endo
import Control.Concurrent
import Control.Concurrent.Chan
import Dna
import Rna

printRnaPipe :: Chan RNA -> IO ()
printRnaPipe rnapipe = do rna <- readChan rnapipe
                          putStrLn $ show rna
                          printRnaPipe rnapipe

main :: IO ()
main = do contents <- readFile "endo.dna"
          let dna = toDna contents
          rnapipe <- newChan
          forkIO $ execute rnapipe dna
          drawRna rnapipe
