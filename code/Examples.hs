import Prelude hiding (filter, lookup, sequence_)
import Control.Concurrent

dollarRate = 1.17047

-- |convert EUR to USD
toUsd euros = euros * dollarRate

toEuro usd = usd / dollarRate

prop_euroUsd x = toEuro (toUsd x) == x

prop_euroUsd' x =
    let inUsd = toUsd x
        backInEuro = toEuro inUsd
        delta = backInEuro - x
    in abs delta < 10e-10

absolute x | x >= 0 = x
absolute x | x < 0  = -x

absolute' x | x >= 0 = x
            | x < 0  = -x

absolute'' x =
    if x >= 0 then x else -x

-- compute x to n-th power
power :: Integer -> Integer -> Integer
power x 0         = 1
power x n | n > 0 = x * power x (n - 1)

fac :: Integer -> Integer
fac i = if i <= 0 then 1 else i * (fac (i - 1))

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

sumTheBools :: (Bool -> Int) -> Int
sumTheBools fun = fun True + fun False

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter pred (x:xs)
    | pred x    = x : filter pred xs
    | otherwise = filter pred xs

lookup :: Eq a => a -> [(a, b)] -> Maybe b
lookup _ [] = Nothing
lookup k ((x, y) : rest)
    | k == x    = Just y
    | otherwise = lookup k rest

printName :: IO ()
printName =
    do putStr "What's your name? "
       s <- getLine
       putStrLn ("Ah, your name is " ++ s)

sequence_ :: [IO ()] -> IO ()
sequence_ [] = return ()
sequence_ (a:as) =
    do a
       sequence_ as

ioIf :: IO Bool -> IO a -> IO a -> IO a
ioIf condAction ifAction thenAction =
    do cond <- condAction
       if cond then ifAction else thenAction

threadExample :: IO ()
threadExample =
    do forkIO (doWork "x" 10)
       forkIO (doWork "y" 10)
       return ()
    where
      doWork x i =
          do putStr x
             threadDelay 100000
             if i < 1 then return () else doWork x (i - 1)

data Circle = Circle Double
    deriving (Show)

data Color = Red | Green | Blue
    deriving (Show)

data Rectangle = Rectangle { r_width :: Double, r_height :: Double }
    deriving (Ord, Show)

instance Eq Rectangle where
    r1 == r2 =
        r_width r1 == r_width r2 &&
        r_height r1 == r_height r2

data Shape
    = ShapeRectangle Rectangle
    | ShapeCircle Circle
    | ShapeAbove Shape Shape
    | ShapeBeside Shape Shape
    | ColoredShape Color Shape
    deriving (Show)

sampleShape =
    ShapeAbove
        (ShapeCircle (Circle 10))
        (ColoredShape Red (ShapeRectangle
            (Rectangle { r_width = 10, r_height = 20 })))

countCircles :: Shape -> Int
countCircles shape =
    case shape of
      ShapeRectangle _ -> 0
      ShapeCircle _ -> 1
      ShapeAbove top bottom ->
          countCircles top + countCircles bottom
      ShapeBeside left right ->
          countCircles left + countCircles right
      ColoredShape _ shape ->
          countCircles shape

boundingBox :: Shape -> Rectangle
boundingBox shape =
    case shape of
      ShapeRectangle rect -> rect
      ShapeCircle (Circle rad) -> Rectangle rad rad
      ShapeAbove top bottom ->
          let topBb = boundingBox top
              bottomBb = boundingBox bottom
          in Rectangle
                 { r_width = max (r_width topBb) (r_width bottomBb)
                 , r_height = r_height topBb + r_height bottomBb
                 }
      ShapeBeside left right ->
          let leftBb = boundingBox left
              rightBb = boundingBox right
          in Rectangle
                 { r_width = r_width leftBb + r_width rightBb
                 , r_height = max (r_height leftBb) (r_height rightBb)
                 }
      ColoredShape _ shape ->
          boundingBox shape
