import Endo
import Control.Concurrent
import Control.Concurrent.Chan
import Dna
import Rna

main :: IO ()
main = do contents <- readFile "/Users/iain/icfp/icfp2007/endo.dna"
          let dna = toDna contents
          rnapipe <- newChan
          forkIO $ execute rnapipe dna
          drawRna rnapipe
