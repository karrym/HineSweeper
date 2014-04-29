
module Main where

import System.Random
import Control.Monad
import Control.Monad.Trans.State
import Data.Array
import Control.Applicative
import Control.Arrow
import Graphics.Gloss.Interface.Pure.Game

data Block = Hidden Mass
           | Open Mass
           | Flag Mass
           deriving (Eq, Show)

data Mass = Bomb
          | Num Int
          deriving (Eq, Show)

type Pos = (Int, Int)

type Field = Array Pos Block

width, height, size, mine :: Int
(width, height, size, mine) = (30, 16, 20, 50)

around :: Pos -> [Pos]
around (x, y) = [(x + 1, y - 1), (x + 1, y), (x + 1, y + 1),
                 (x, y - 1), (x, y + 1),
                 (x - 1, y - 1), (x - 1, y), (x - 1, y + 1)]

newField :: [Pos] -> Field
newField ps = initial $ accumArray seq (Hidden (Num 0)) ((0, 0), (width+2, height+2)) (field ps []) where
    field (x:xs) list | length list == mine = list
                      | otherwise = if elem x (map fst list) 
                                        then field xs list
                                        else field xs $ (x, Hidden Bomb) : list
    initial ary = ary // do
        x <- [1..width]
        y <- [1..height]
        guard $ ary ! (x, y) /= Hidden Bomb
        let num = length $ do
            (px, py) <- around (x, y)
            guard $ ary ! (px, py) == Hidden Bomb
        return ((x, y), Hidden $ Num num)

update :: Float -> State Field ()
update = const $ return ()

transPos :: (Float, Float) -> Pos
transPos (x, y) = (floor x `div` size + width `div` 2 + 1, floor y `div` size + height `div` 2 + 1)

event :: Event -> State Field ()
event (EventKey (MouseButton LeftButton) Down _ p) = openAt $ transPos p
event (EventKey (MouseButton RightButton) Down _ p) = do
        ary <- get
        let pos = transPos p
        case ary ! pos of
            Hidden n -> put $ ary // [(pos, Flag n)]
            Flag n -> put $ ary // [(pos, Hidden n)]
            Open _ -> return ()
event _ = return ()

vectors :: [Pos]
vectors = [(-1, -1),(-1, 0),(-1, 1),(0, -1),(0, 1),(1, -1),(1, 0),(1, 1)]

addv :: Pos -> Pos -> Pos
addv (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

isInside :: Pos -> Bool
isInside (x, y) = x >= 1 && x <= width && y >= 1 && y <= height

openAt :: Pos -> State Field ()
openAt p = when (isInside p) $ do
    ary <- get
    case ary ! p of
        Open _ -> return ()
        Flag _ -> return ()
        Hidden n -> do
            put $ ary // [(p, Open n)]
            when (n == Num 0) $ mapM_ openAt $ map (addv p) vectors

draw :: Field -> Picture
draw field = pictures $ do
    x <- [1..width]
    y <- [1..height]
    let pos = (fromIntegral $ (x - width `div` 2) * size - size `div` 2,
               fromIntegral $ (y - height `div` 2) * size - size `div` 2)
    return . uncurry translate pos . drawBlock $ field ! (x, y)

numPicture :: Int -> Picture
numPicture n = color (numColor n) $ translate (fromIntegral (-size)) (fromIntegral (-size) * 5 / 6) $ text $ show n

numColor :: Int -> Color
numColor 1 = blue
numColor 2 = makeColor8 0 128 0 1
numColor 3 = red
numColor 4 = makeColor8 22 94 131 1
numColor 5 = makeColor8 153 76 0 1
numColor 6 = makeColor8 0 144 168 1
numColor 7 = black
numColor 8 = greyN 0.3

drawBlock :: Block -> Picture
drawBlock (Hidden _) = pictures [color blue $ rectangleSolid (fromIntegral size) (fromIntegral size),
                                color black $ rectangleWire (fromIntegral size) (fromIntegral size)]
drawBlock (Flag _) = pictures [color red $ rectangleSolid (fromIntegral size) (fromIntegral size),
                                color black $ rectangleWire (fromIntegral size) (fromIntegral size)]
drawBlock (Open (Num 0)) = color (greyN 0.75) $ rectangleSolid (fromIntegral size) (fromIntegral size)
drawBlock (Open (Num n)) = scale 0.1 0.1 $ numPicture n
drawBlock (Open Bomb) = color black $ rectangleSolid (fromIntegral size) (fromIntegral size)

fps :: Int
fps = 50

toPos :: [Int] -> [Pos]
toPos (x:y:xs) = (x, y) : toPos xs

main :: IO ()
main = do
        rand <- map (succ . flip mod width *** succ . flip mod height) . toPos . randomRs (0, max width height) <$> newStdGen
        play (InWindow "MineSweeper" (width * size, height * size) (10, 10))
            (greyN 0.75)
            fps
            (newField rand)
            draw
            (execState . event)
            (execState . update)
