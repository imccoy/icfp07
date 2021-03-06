module Rna (drawRna, Bitmap, explode, transparentBitmap) where

import Endo
import Control.Concurrent.Chan
import Data.Array
import Data.Foldable (toList)
import Data.List (foldl')
import qualified Data.Set as Set
import System.Exit
import Debug.Trace

import qualified Graphics.UI.WX as Wx
import Graphics.UI.WX.Attributes (Prop ((:=)))

type Coord = Int
type Pos = (Coord, Coord)

type Component = Int
type RGB = (Component, Component, Component)
type Transparency = Component
type Pixel = (RGB, Transparency)

type Bitmap = Array Pos Pixel
type Canvas = ([(Pos,Pos,Pixel)],Bitmap)

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

type DrawState = (Bucket, Pos, Pos, Dir, [Canvas])
getBucket  (b, _, _, _, _) = b
getPos     (_, p, _, _, _) = p
getMark    (_, _, m, _, _) = m
getDir     (_, _, _, d, _) = d
getBitmaps (_, _, _, _, b) = b

adjustBucket  f (b, x, x1, x2, x3) = (f b, x,   x1,  x2,  x3)
adjustPos     f (x, p, x1, x2, x3) = (x,   f p, x1,  x2,  x3)
adjustMark    f (x, x1, m, x2, x3) = (x,   x1,  f m, x2,  x3)
adjustDir     f (x, x1, x2, d, x3) = (x,   x1,  x2,  f d, x3)
adjustBitmaps f (x, x1, x2, x3, b) = (x,   x1,  x2,  x3, f b)
adjustBitmapsS f (x, x1, x2, x3, b) = b' `seq` (x,   x1,  x2,  x3, b')
  where b' = f b

strictDrawState ds@(a,b,c,d,e) = a `seq` b `seq` c `seq` d `seq` e `seq` (a,b,c,d,e)

canvasSize = 600

transparentBitmap n = ([], array ((0, 0),(n-1,n-1)) $ zip [(x, y) | x <- [0..n-1], y <- [0..n-1]] (repeat transparentPixel))
  where transparentPixel = (black, transparent)

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

adjustTop :: (Canvas -> Canvas) -> [Canvas] -> [Canvas]
adjustTop f (x:xs) = let x' = f x 
                      in x' `seq` (x':xs)

flatten [] = []
flatten ([]:xss) = flatten xss
flatten ((x:xs):xss) = x:(flatten (xs:xss))

flattenCanvas :: Canvas -> Bitmap
flattenCanvas (ls, b) = b // (flatten $ map settersFromLine ls)
  where settersFromLine ((x0,y0), (x1,y1), p) = let deltax = x1 - x0
                                                    deltay = y1 - y0
                                                    d = max (abs deltax) (abs deltay)
                                                    c = if deltax * deltay <= 0 then 1 else 0
                                                    x = x0 * d + ((d - c) `div` 2)
                                                    y = y0 * d + ((d - c) `div` 2)
                                                    addDeltaDivideD delta x = map (\a -> div a d) $ iterate ((+) delta) x
                                                    xs' = addDeltaDivideD deltax x
                                                    ys' = addDeltaDivideD deltay y
                                                    xys' = take d $ zip xs' ys'
                                                 in zip ((x1,y1):xys') (repeat p)




line :: Pixel -> Pos -> Pos -> Canvas -> Canvas
line p start end (ls, bmp) = ((start, end, p):ls, bmp)

drawLine :: DrawState -> Canvas -> Canvas
drawLine ds = line (currentPixel $ getBucket ds) (getPos ds) (getMark ds)

canvasSizeFromArray bitmap = snd (bounds bitmap)

getAdjacentPositionsWithColor initial bitmap (x,y) = getAdjacentPositionsWithColor' (Set.empty, [(x,y)], Set.empty)
  where addCoord (-1,_) coords = coords
        addCoord (_,-1) coords = coords
        addCoord (600,_) coords = coords
        addCoord (_,600) coords = coords
        addCoord pos (queue, visited)
          | Set.member pos visited = (queue, visited)
          | otherwise              = (pos:queue, Set.insert pos visited)
        getAdjacentPositionsWithColor' (coords, [], visited) = coords
        getAdjacentPositionsWithColor' (coords, (pos@(x,y):queue), visited)
          | getPixel pos bitmap == initial = let q1 = addCoord (x,y+1) (queue,visited)
                                                 q2 = addCoord (x,y-1) q1
                                                 q3 = addCoord (x+1,y) q2
                                                 (queue4, visited4) = addCoord (x-1,y) q3
                                              in getAdjacentPositionsWithColor' (Set.insert pos coords, queue4, visited4)
          | otherwise                      =  getAdjacentPositionsWithColor' (coords, queue, visited)

setAll coords p bitmap = bitmap // zip (Set.toList coords) (repeat p)

fill (x,y) initial current bitmap = setAll (getAdjacentPositionsWithColor initial bitmap (x,y)) current bitmap

tryFill position initial current
  | initial /= current = fill position initial current
  | otherwise          = id

drawFill :: DrawState -> Canvas -> Canvas
drawFill ds canvas = newCanvas `seq` ([], newCanvas)
  where old = getPixel (getPos ds) (flattenCanvas $ head $ getBitmaps ds)
        new = currentPixel $ getBucket ds
	drawFillBmp = tryFill (getPos ds) old new
        newCanvas = drawFillBmp $ flattenCanvas canvas

setPixel :: Pixel -> Pos -> Bitmap -> Bitmap
setPixel pixel (x,y) bitmap  = bitmap // [((x,y),pixel)]

getPixel :: Pos -> Bitmap -> Pixel
getPixel (x, y) bitmap = bitmap ! (x,y)


addBitmap bitmaps@(a1:a2:a3:a4:a5:a6:a7:a8:a9:a10:[]) = bitmaps
addBitmap bitmaps = (transparentBitmap canvasSize):bitmaps

allPos s = zip [0..s] [0..s]

canvasFromTwo :: Canvas -> Canvas -> (Bitmap -> Bitmap -> Bitmap) -> Canvas
canvasFromTwo c1 c2 f = c' `seq` ([], c')
  where c' = f (flattenCanvas c1) (flattenCanvas c2)

mixArrays :: ((Pixel, Pixel) -> Pixel) -> Bitmap -> Bitmap -> Bitmap
mixArrays f a b = let es = forceMap f (zip (elems a) (elems b))
                   in es `seq` listArray (bounds a) $ es

forceMap :: (a -> b) -> [a] -> [b]
forceMap f [] = []
forceMap f (a:as) = let a' = f a
                     in a' `seq` a':(forceMap f as)


compose (bm0:bm1:bitmaps) = newCanvas `seq` newCanvas:bitmaps
  where compose' bm0 bm1 = mixArrays composePixel bm0 bm1
        composePixel (((r0,g0,b0),a0),((r1,g1,b1),a1)) = r' `seq` g' `seq` b' `seq` a' `seq`  ((r', g', b'), a')
            where combine n0 n1 = n0 + n1 * (255 - a0) `div` 255
                  r' = combine r0 r1
                  g' = combine g0 g1
                  b' = combine b0 b1
                  a' = combine a0 a1
        newCanvas = canvasFromTwo bm0 bm1 compose'
compose bitmaps = bitmaps


clip (bm0:bm1:bitmaps) = newCanvas `seq` newCanvas:bitmaps
  where clip' bm0 bm1 = mixArrays clipPixel bm0 bm1
        clipPixel (((r0,g0,b0),a0),((r1,g1,b1),a1)) = r' `seq` g' `seq` b' `seq` a' `seq`  ((r', g', b'), a')
            where fade c = c * a0 `div` 255
                  r' = fade r1
                  g' = fade g1
                  b' = fade b1
                  a' = fade a1
        newCanvas = canvasFromTwo bm0 bm1 clip'
clip bitmaps = bitmaps

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
applyRNA [P,F,F,F,F,F,P] = adjustDir turnClockwise

applyRNA [P,C,C,I,F,F,P] = (\ds -> adjustMark (\_ -> getPos ds) ds)
applyRNA [P,F,F,I,C,C,P] = (\ds -> adjustBitmapsS (adjustTop $ drawLine ds) ds)
applyRNA [P,I,I,P,I,I,P] = (\ds -> adjustBitmapsS (adjustTop $ drawFill ds) ds)

applyRNA [P,C,C,P,F,F,P] = adjustBitmaps addBitmap
applyRNA [P,F,F,P,C,C,P] = adjustBitmapsS compose
applyRNA [P,F,F,I,C,C,F] = adjustBitmaps clip

applyRNA _               = id

drawRna :: Chan (Maybe RNA) -> IO ()
drawRna rnapipe = do let ds = initialDrawState
                     drawRna' rnapipe ds

drawRna' rnapipe ds = do rnaMsg <- readChan rnapipe
                         case rnaMsg of
                           Just rna -> drawRna' rnapipe $! strictDrawState $ applyRNA (toList rna) ds
                           Nothing -> render ds

paintBitmap bitmap dc viewArea = mapM_ paintPixel (assocs bitmap)
  where paintPixel ((x,y),((r,g,b),a)) = do Wx.set dc [Wx.penColor := Wx.colorRGB r g b]
                                            Wx.drawPoint dc (Wx.pt x y) []

makeWindow bitmap = do f <- Wx.frameFixed [Wx.text := "Endo"]
                       p <- Wx.panel f [Wx.on Wx.paint := paintBitmap bitmap]
                       Wx.set f [Wx.layout := Wx.minsize (Wx.sz canvasSize canvasSize) $ Wx.widget p]

-- drawRna' ds rnapipe = do rnaMsg <- readChan rnapipe
--                          putStrLn $ show rnaMsg
--                          drawRna' ds rnapipe


-- render ds = do --putStrLn $ show $ head $ getBitmaps ds
--               exitWith ExitSuccess

render ds = do Wx.start $ makeWindow (flattenCanvas $ head $ getBitmaps ds)

