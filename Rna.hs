module Rna (drawRna, Bitmap) where

import Endo
import Control.Concurrent.Chan
import Data.Array
import Data.Foldable (toList)
import Data.List (foldl')
import System.Exit

import qualified Graphics.UI.WX as Wx
import Graphics.UI.WX.Attributes (Prop ((:=)))

type Coord = Int
type Pos = (Coord, Coord)

type Component = Int
type RGB = (Component, Component, Component)
type Transparency = Component
type Pixel = (RGB, Transparency)

type Bitmap = Array Coord (Array Coord Pixel)

data Color = Col_RGB RGB | Col_Transparency Transparency
  deriving (Show)
type Bucket = [Color]

data Dir = N | E | S | W
  deriving (Show)

black    = (0,   0,   0  )
red      = (255, 0,   0  )
green    = (0,   255, 0  )
yellow   = (255, 255, 0  )
blue     = (0,   0,   255)
magenta  = (255, 0,   255)
cyan     = (0,   255, 255)
white    = (255, 255, 255)

transparent = 0
opaque      = 255

type DrawState = (Bucket, Pos, Pos, Dir, [Bitmap])
getBucket  (b, _, _, _, _) = b
getPos     (_, p, _, _, _) = p
getMark    (_, _, m, _, _) = m
getDir     (_, _, _, d, _) = d
getBitmaps (_, _, _, _, b) = b

adjustBucket  f (b, x, x1, x2, x3) = (f b, x,   x1,  x2,  x3)
adjustPos     f (x, p, x1, x2, x3) = (x,   f p, x1,  x2,  x3)
adjustMark    f (x, x1, m, x2, x3) = (x,   x1,  f m, x2,  x3)
adjustDir     f (x, x1, x2, d, x3) = (x,   x1,  x2,  f d, x3)
adjustBitmaps :: ([Bitmap] -> [Bitmap]) -> DrawState -> DrawState
adjustBitmaps f (x, x1, x2, x3, b) = (x,   x1,  x2,  x3, f b)

canvasSize = 600

transparentBitmap n = array (0, n-1) $ zip [0..n-1] (repeat transparentRow)
  where transparentPixel = (black, transparent)
        transparentRow =   array (0, n-1)  $ zip [0..n-1] (repeat transparentPixel)

initialDrawState :: DrawState
initialDrawState = ([], (0, 0), (0, 0), E, [transparentBitmap canvasSize])

addColor :: Color -> Bucket -> Bucket
addColor = (:)

currentPixel :: Bucket -> Pixel
currentPixel bucket = ((r * a `div` 255, g * a `div` 255, b * a `div` 255), a)
                      where addUp bucket = addUp' ((0,(0,0,0)),(0,0)) bucket
                            addUp' ((n,(r,g,b)),trans) ((Col_RGB (r',g',b')):bucket) = addUp' ((n+1,(r+r',g+g',b+b')),trans)   bucket
                            addUp' (cols,       (n,a)) ((Col_Transparency a'):bucket) = addUp' (cols,                  (n+1,a+a')) bucket
                            addUp' (cols, trans)       []                            = (cols, trans)
                            ((ncol,(rs, gs, bs)), (nt,as)) = addUp bucket
                            avg 0 0 d = d
                            avg s n d = s `div` n
                            r = avg rs ncol 0
                            g = avg gs ncol 0
                            b = avg bs ncol 0
                            a = avg as nt 255

currentPixel2 :: Bucket -> Pixel
currentPixel2 bucket = ((alph (avg rs 0), alph (avg gs 0), alph (avg bs 0)), avg trans 255)
                       where (cols, trans) = splitBucket [] [] bucket
                             splitBucket cols trans ((Col_RGB rgb):bucket) = splitBucket (rgb:cols) trans bucket
                             splitBucket cols trans ((Col_Transparency t):bucket) = splitBucket cols (t:trans) bucket
                             splitBucket cols trans [] = (cols, trans)
                             rs = map (\(r,_,_) -> r) cols
                             gs = map (\(_,g,_) -> g) cols
                             bs = map (\(_,_,b) -> b) cols
                             avg [] d = d
                             avg x  d = (sum x) `div` (length x)
                             alph c = c * (avg trans 255) `div` 255

exampleBucket1 = [Col_Transparency transparent, Col_Transparency opaque, Col_Transparency opaque]
exampleBucket2 = [Col_RGB blue, Col_RGB yellow, Col_RGB cyan]
exampleBucket3 = [Col_RGB yellow, Col_Transparency transparent, Col_Transparency opaque]
                             

adjust (d1, d2) (x1, x2) = ((x1 + d1) `mod` canvasSize, (x2 + d2) `mod` canvasSize) 

move :: Dir -> Pos -> Pos
move E = adjust (1,  0)
move S = adjust (0,  1)
move W = adjust (-1, 0)
move N = adjust (0, -1)

turnCounterClockwise N = W
turnCounterClockwise E = N
turnCounterClockwise S = E
turnCounterClockwise W = S

turnClockwise N = E
turnClockwise E = S
turnClockwise S = W
turnClockwise W = N

adjustTop :: (Bitmap -> Bitmap) -> [Bitmap] -> [Bitmap]
adjustTop f (x:xs) = (f x):xs

line :: Pixel -> Pos -> Pos -> Bitmap -> Bitmap
line p (x0, y0) (x1, y1) b = let deltax = x1 - x0
                                 deltay = y1 - y0
                                 d = max (abs deltax) (abs deltay)
                                 c = if deltax * deltay <= 0 then 1 else 0
                                 x = x0 * d + ((d - c) `div` 2)
                                 y = y0 * d + ((d - c) `div` 2)
                                 xs = iterate ((+) deltax) x
                                 ys = iterate ((+) deltay) y
                              in setPixel p (x1,y1) $ foldl' (\pix (x,y) -> setPixel p (x `div` d, y `div` d) pix) b $ take d (zip xs ys)

drawLine :: DrawState -> (Bitmap -> Bitmap)
drawLine ds = line (currentPixel $ getBucket ds) (getPos ds) (getMark ds)

canvasSizeFromArray bitmap = snd (bounds bitmap)

fill (x,y) initial current bitmap
  | getPixel (x,y) bitmap  == initial = recurseLeft . recurseRight . recurseUp . recurseDown $ (setPixel current (x,y)) bitmap
  | otherwise                         = bitmap
                                        where maxval = canvasSizeFromArray bitmap
                                              recurse (xd, yd)
                                                 | x + xd >= 0 && x + xd <= maxval && y + yd >= 0 && y + yd <= maxval = fill (x + xd, y + yd) initial current
                                                 | otherwise                                                          = id
                                              recurseLeft = recurse (-1, 0)
                                              recurseRight = recurse (1, 0)
                                              recurseUp = recurse (0, -1)
                                              recurseDown = recurse (0,1)

tryFill position initial current
  | initial /= current = fill position initial current
  | otherwise          = id

drawFill :: DrawState -> Bitmap -> Bitmap
drawFill ds = tryFill (getPos ds) (getPixel (getPos ds) (head $ getBitmaps ds)) (currentPixel $ getBucket ds)

setPixel :: Pixel -> Pos -> Bitmap -> Bitmap
setPixel pixel (x,y) bitmap  = let row = bitmap ! x
                                   newRow = row // [(y,pixel)]
                                in bitmap // [(x,newRow)]

getPixel :: Pos -> Bitmap -> Pixel
getPixel (x, y) bitmap = (bitmap ! x) ! y

addBitmap bitmaps = (transparentBitmap canvasSize):bitmaps

allPos s = zip [0..s] [0..s]

compose (bm0:bm1:bitmaps) = (compose' bm0 bm1):bitmaps
  where compose' bm0 bm1 = foldl' composePixel bm1 (allPos (canvasSizeFromArray bm1))
        composePixel bm1 pos = let ((r0, g0, b0), a0) = getPixel pos bm0
                                   ((r1, g1, b1), a1) = getPixel pos bm1
                                   combine n0 n1 = n0 + n1 * (255 - a0) `div` 255
                                   pix = ((combine r0 r1, combine g0 g1, combine b0 b1), combine a0 a1)
                                in setPixel pix pos bm1
compose bitmaps = bitmaps


clip (bm0:bm1:bitmaps) = (clip' bm0 bm1):bitmaps
  where clip' bm0 bm1 = foldl' clipPixel bm1 (allPos (canvasSizeFromArray bm1))
        clipPixel bm1 pos = let ((r0, g0, b0), a0) = getPixel pos bm0
                                ((r1, g1, b1), a1) = getPixel pos bm1
                                fade c = c * a0 `div` 255
                                pix = ((fade r1, fade g1, fade b1), fade a1)
                             in setPixel pix pos bm1

applyRNA ::  [Base] -> DrawState -> DrawState
applyRNA [P,I,P,I,I,I,C] = adjustBucket (addColor $ Col_RGB black)
applyRNA [P,I,P,I,I,I,P] = adjustBucket (addColor $ Col_RGB red)
applyRNA [P,I,P,I,I,C,C] = adjustBucket (addColor $ Col_RGB green)
applyRNA [P,I,P,I,I,C,F] = adjustBucket (addColor $ Col_RGB yellow)
applyRNA [P,I,P,I,I,C,P] = adjustBucket (addColor $ Col_RGB blue)
applyRNA [P,I,P,I,I,F,C] = adjustBucket (addColor $ Col_RGB magenta)
applyRNA [P,I,P,I,I,F,F] = adjustBucket (addColor $ Col_RGB cyan)
applyRNA [P,I,P,I,I,P,C] = adjustBucket (addColor $ Col_RGB white)
applyRNA [P,I,P,I,I,P,F] = adjustBucket (addColor $ Col_Transparency transparent)
applyRNA [P,I,P,I,I,P,P] = adjustBucket (addColor $ Col_Transparency opaque)
applyRNA [P,I,I,P,I,C,P] = adjustBucket (\_ -> [])

applyRNA [P,I,I,I,I,I,P] = (\ds -> adjustPos (move $ getDir ds) ds)
applyRNA [P,C,C,C,C,C,P] = adjustDir turnCounterClockwise
applyRNA [P,F,F,F,F,F,P] = adjustDir turnCounterClockwise

applyRNA [P,C,C,I,F,F,P] = (\ds -> adjustMark (\_ -> getPos ds) ds)
applyRNA [P,F,F,I,C,C,P] = (\ds -> adjustBitmaps (adjustTop $ drawLine ds) ds)
applyRNA [P,I,I,P,I,I,P] = (\ds -> adjustBitmaps (adjustTop $ drawFill ds) ds)

applyRNA [P,C,C,P,F,F,P] = adjustBitmaps addBitmap
applyRNA [P,F,F,P,C,C,P] = adjustBitmaps compose
applyRNA [P,F,F,I,C,C,F] = adjustBitmaps clip

drawRna :: Chan (Maybe RNA) -> IO ()
drawRna rnapipe = do let ds = initialDrawState
                     drawRna' rnapipe ds

drawRna' rnapipe ds = do rnaMsg <- readChan rnapipe
                         case rnaMsg of
                           Just rna ->
                             drawRna' rnapipe $! applyRNA (toList rna) ds
                           Nothing -> 
                             do render ds

paintBitmap bitmap dc viewArea = mapM_ paintRow (assocs bitmap)
  where paintPixel (r,g,b) x y = do Wx.set dc [Wx.penColor := Wx.colorRGB r g b]
                                    Wx.drawPoint dc (Wx.pt x y) []
        paintRow (y, row_array) = mapM_ (\(x, (rgb,a)) -> paintPixel rgb x y) (assocs row_array) 

makeWindow bitmap = do f <- Wx.frameFixed [Wx.text := "Endo"]
                       p <- Wx.panel f [Wx.on Wx.paint := paintBitmap bitmap]
                       Wx.set f [Wx.layout := Wx.minsize (Wx.sz canvasSize canvasSize) $ Wx.widget p]

-- drawRna' ds rnapipe = do rnaMsg <- readChan rnapipe
--                          putStrLn $ show rnaMsg
--                          drawRna' ds rnapipe
-- 

render ds = do Wx.start $ makeWindow (head $ getBitmaps ds)
