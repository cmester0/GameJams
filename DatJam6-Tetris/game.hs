{-# LANGUAGE OverloadedStrings #-}
module Main where

import SDL hiding (get)
import SDL.Time 
import Linear (V4(..))
import Control.Monad (unless)
import Control.Monad.State
import Control.Concurrent
import Foreign.C.Types

import System.Random

xStepSize = 40
yStepSize = 40

screenWidth = 10 * (xStepSize + 2) + 2
screenHeight = 22 * (yStepSize + 2) + 2

main :: IO ()
main = do
  initializeAll
  window <- createWindow "Tetris" (defaultWindow {windowInitialSize = V2 screenWidth screenHeight})
  renderer <- createRenderer window (-1) defaultRenderer
  appLoop renderer (do a <- get
                       return a)

data Piece = O | I | S | Z | T | L | J deriving (Bounded, Enum) 
instance Random Piece where
    random g = case randomR (fromEnum (minBound :: Piece), fromEnum (maxBound :: Piece)) g of
                 (r, g') -> (toEnum r, g')
    randomR (a,b) g = case randomR (fromEnum a, fromEnum b) g of
                        (r, g') -> (toEnum r, g')

data Rotation = RStandard | RLeft | RRight | RInverted
                        
type InnerState = (Bool , V2 CInt, [[Bool]], Int, Int, Piece, Rotation)
type GameState = State InnerState InnerState

tupleToRectangle (x,y,w,h) = Rectangle (P (V2 x y)) (V2 w h)

initState :: InnerState
initState = (False ,
             V2 4 0,
             [0 .. 21] >>= \y -> [([0 .. 9] >>= \x -> [False])],
             100,
             100,
             L, -- O
             RStandard)

posToSquare :: V2 CInt -> Rectangle CInt
posToSquare (V2 x y) = Rectangle (P (V2 ((fromInteger (toInteger x)) *  (2 + xStepSize) + 2) ((fromInteger (toInteger y)) * (2 + yStepSize) + 2))) (V2 (xStepSize) (yStepSize))

renderType renderer p O _ =
  do
    rendererDrawColor renderer $= V4 255 0 255 255
    (fillRect renderer . Just) (posToSquare p)
    (fillRect renderer . Just) (posToSquare (p + V2 0 (-1)))
    (fillRect renderer . Just) (posToSquare (p + V2 1 (-1)))
    (fillRect renderer . Just) (posToSquare (p + V2 1 0))
    
renderType renderer p I RStandard =
  do
    rendererDrawColor renderer $= V4 255 0 255 255
    (fillRect renderer . Just) (posToSquare p)
    (fillRect renderer . Just) (posToSquare (p + V2 0 (-1)))
    (fillRect renderer . Just) (posToSquare (p + V2 0 (-2)))
    (fillRect renderer . Just) (posToSquare (p + V2 0 (-3)))
renderType renderer p I RInverted =
  do
    rendererDrawColor renderer $= V4 255 0 255 255
    (fillRect renderer . Just) (posToSquare p)
    (fillRect renderer . Just) (posToSquare (p + V2 0 (-1)))
    (fillRect renderer . Just) (posToSquare (p + V2 0 (-2)))
    (fillRect renderer . Just) (posToSquare (p + V2 0 (-3)))
renderType renderer p I RLeft =
  do
    rendererDrawColor renderer $= V4 255 0 255 255
    (fillRect renderer . Just) (posToSquare (p + V2 1 0))
    (fillRect renderer . Just) (posToSquare p)
    (fillRect renderer . Just) (posToSquare (p + V2 (-1) 0))
    (fillRect renderer . Just) (posToSquare (p + V2 (-2) 0))
renderType renderer p I RRight =
  do
    rendererDrawColor renderer $= V4 255 0 255 255
    (fillRect renderer . Just) (posToSquare (p + V2 (-1) 0))
    (fillRect renderer . Just) (posToSquare p)
    (fillRect renderer . Just) (posToSquare (p + V2 1 0))
    (fillRect renderer . Just) (posToSquare (p + V2 2 0))

renderType renderer p S RStandard =
  do
    rendererDrawColor renderer $= V4 255 0 255 255
    (fillRect renderer . Just) (posToSquare p)
    (fillRect renderer . Just) (posToSquare (p + V2 (-1) 0))
    (fillRect renderer . Just) (posToSquare (p + V2 0 (-1)))
    (fillRect renderer . Just) (posToSquare (p + V2 1 (-1)))
renderType renderer p S RLeft =
  do
    rendererDrawColor renderer $= V4 255 0 255 255
    (fillRect renderer . Just) (posToSquare p)
    (fillRect renderer . Just) (posToSquare (p + V2 0 (-1)))
    (fillRect renderer . Just) (posToSquare (p + V2 (-1) (-1)))
    (fillRect renderer . Just) (posToSquare (p + V2 (-1) (-2)))
renderType renderer p S RInverted = renderType renderer p S RStandard
renderType renderer p S RRight = renderType renderer p S RLeft

renderType renderer p Z RStandard =
  do
    rendererDrawColor renderer $= V4 255 0 255 255
    (fillRect renderer . Just) (posToSquare p)
    (fillRect renderer . Just) (posToSquare (p + V2 1 0))
    (fillRect renderer . Just) (posToSquare (p + V2 0 (-1)))
    (fillRect renderer . Just) (posToSquare (p + V2 (-1) (-1)))
renderType renderer p Z RLeft =
  do
    rendererDrawColor renderer $= V4 255 0 255 255
    (fillRect renderer . Just) (posToSquare p)
    (fillRect renderer . Just) (posToSquare (p + V2 0 (-1)))
    (fillRect renderer . Just) (posToSquare (p + V2 1 (-1)))
    (fillRect renderer . Just) (posToSquare (p + V2 1 (-2)))
renderType renderer p Z RInverted = renderType renderer p Z RStandard
renderType renderer p Z RRight = renderType renderer p Z RLeft

renderType renderer p T RStandard =
  do
    rendererDrawColor renderer $= V4 255 0 255 255
    (fillRect renderer . Just) (posToSquare p)
    (fillRect renderer . Just) (posToSquare (p + V2 1 0))
    (fillRect renderer . Just) (posToSquare (p + V2 (-1) 0))
    (fillRect renderer . Just) (posToSquare (p + V2 0 (-1)))
renderType renderer p T RLeft = 
  do
    rendererDrawColor renderer $= V4 255 0 255 255
    (fillRect renderer . Just) (posToSquare p)
    (fillRect renderer . Just) (posToSquare (p + V2 0 (-1)))
    (fillRect renderer . Just) (posToSquare (p + V2 0 (-2)))
    (fillRect renderer . Just) (posToSquare (p + V2 (-1) (-1)))
renderType renderer p T RInverted = 
  do
    rendererDrawColor renderer $= V4 255 0 255 255
    (fillRect renderer . Just) (posToSquare p)
    (fillRect renderer . Just) (posToSquare (p + V2 1 (-1)))
    (fillRect renderer . Just) (posToSquare (p + V2 0 (-1)))
    (fillRect renderer . Just) (posToSquare (p + V2 (-1) (-1)))
renderType renderer p T RRight =
  do
    rendererDrawColor renderer $= V4 255 0 255 255
    (fillRect renderer . Just) (posToSquare p)
    (fillRect renderer . Just) (posToSquare (p + V2 1 (-1)))
    (fillRect renderer . Just) (posToSquare (p + V2 0 (-1)))
    (fillRect renderer . Just) (posToSquare (p + V2 0 (-2)))

renderType renderer p L RStandard =
  do
    rendererDrawColor renderer $= V4 255 0 255 255
    (fillRect renderer . Just) (posToSquare p)
    (fillRect renderer . Just) (posToSquare (p + V2 1 0))
    (fillRect renderer . Just) (posToSquare (p + V2 (-1) 0))
    (fillRect renderer . Just) (posToSquare (p + V2 1 (-1)))
renderType renderer p L RLeft = 
  do
    rendererDrawColor renderer $= V4 255 0 255 255
    (fillRect renderer . Just) (posToSquare p)
    (fillRect renderer . Just) (posToSquare (p + V2 0 (-1)))
    (fillRect renderer . Just) (posToSquare (p + V2 0 (-2)))
    (fillRect renderer . Just) (posToSquare (p + V2 (-1) (-2)))
renderType renderer p L RInverted = 
  do
    rendererDrawColor renderer $= V4 255 0 255 255
    (fillRect renderer . Just) (posToSquare (p + V2 (-1) 0))
    (fillRect renderer . Just) (posToSquare (p + V2 (-1) (-1)))
    (fillRect renderer . Just) (posToSquare (p + V2 0 (-1)))
    (fillRect renderer . Just) (posToSquare (p + V2 1 (-1)))
renderType renderer p L RRight =
  do
    rendererDrawColor renderer $= V4 255 0 255 255
    (fillRect renderer . Just) (posToSquare p)
    (fillRect renderer . Just) (posToSquare (p + V2 1 0))
    (fillRect renderer . Just) (posToSquare (p + V2 0 (-1)))
    (fillRect renderer . Just) (posToSquare (p + V2 0 (-2)))

renderType renderer p J RStandard =
  do
    rendererDrawColor renderer $= V4 255 0 255 255
    (fillRect renderer . Just) (posToSquare p)
    (fillRect renderer . Just) (posToSquare (p + V2 1 0))
    (fillRect renderer . Just) (posToSquare (p + V2 (-1) 0))
    (fillRect renderer . Just) (posToSquare (p + V2 (-1) (-1)))
renderType renderer p J RLeft = 
  do
    rendererDrawColor renderer $= V4 255 0 255 255
    (fillRect renderer . Just) (posToSquare p)
    (fillRect renderer . Just) (posToSquare (p + V2 0 (-1)))
    (fillRect renderer . Just) (posToSquare (p + V2 0 (-2)))
    (fillRect renderer . Just) (posToSquare (p + V2 (-1) 0))
renderType renderer p J RInverted = 
  do
    rendererDrawColor renderer $= V4 255 0 255 255
    (fillRect renderer . Just) (posToSquare (p + V2 1 0))
    (fillRect renderer . Just) (posToSquare (p + V2 1 (-1)))
    (fillRect renderer . Just) (posToSquare (p + V2 0 (-1)))
    (fillRect renderer . Just) (posToSquare (p + V2 (-1) (-1)))
renderType renderer p J RRight =
  do
    rendererDrawColor renderer $= V4 255 0 255 255
    (fillRect renderer . Just) (posToSquare p)
    (fillRect renderer . Just) (posToSquare (p + V2 1 (-2)))
    (fillRect renderer . Just) (posToSquare (p + V2 0 (-1)))
    (fillRect renderer . Just) (posToSquare (p + V2 0 (-2)))
    
updateBoardAtXY board px py =
  take py board ++ [(take px (board !! py) ++ [True] ++ drop (px + 1) (board !! py))] ++ drop (py + 1) board

updateBoardWithFigureAtXY board px py O _ =
  updateBoardAtXY (updateBoardAtXY (updateBoardAtXY (updateBoardAtXY board (px + 1) py) px py) px (py - 1)) (px + 1) (py - 1)

updateBoardWithFigureAtXY board px py I RStandard =
  updateBoardAtXY (updateBoardAtXY (updateBoardAtXY (updateBoardAtXY board px py) px (py - 1)) px (py - 2)) px (py - 3)
updateBoardWithFigureAtXY board px py I RLeft =
  updateBoardAtXY (updateBoardAtXY (updateBoardAtXY (updateBoardAtXY board px py) (px + 1) py) (px - 1) py) (px - 2) py
updateBoardWithFigureAtXY board px py I RInverted = updateBoardWithFigureAtXY board px py I RStandard
updateBoardWithFigureAtXY board px py I RRight =
  updateBoardAtXY (updateBoardAtXY (updateBoardAtXY (updateBoardAtXY board px py) (px - 1) py) (px + 2) py) (px + 1) py

updateBoardWithFigureAtXY board px py S RStandard =
  updateBoardAtXY (updateBoardAtXY (updateBoardAtXY (updateBoardAtXY board px py) (px - 1) py) px (py - 1)) (px + 1) (py - 1)
updateBoardWithFigureAtXY board px py S RLeft =
  updateBoardAtXY (updateBoardAtXY (updateBoardAtXY (updateBoardAtXY board px py) px (py - 1)) (px - 1) (py - 1)) (px - 1) (py - 2)
updateBoardWithFigureAtXY board px py S RInverted = updateBoardWithFigureAtXY board px py S RStandard
updateBoardWithFigureAtXY board px py S RRight = updateBoardWithFigureAtXY board px py S RLeft

updateBoardWithFigureAtXY board px py Z RStandard =
  updateBoardAtXY (updateBoardAtXY (updateBoardAtXY (updateBoardAtXY board px py) (px + 1) py) px (py - 1)) (px - 1) (py - 1)
updateBoardWithFigureAtXY board px py Z RLeft =
  updateBoardAtXY (updateBoardAtXY (updateBoardAtXY (updateBoardAtXY board px py) px (py - 1)) (px + 1) (py - 1)) (px + 1) (py - 2)
updateBoardWithFigureAtXY board px py Z RInverted = updateBoardWithFigureAtXY board px py Z RStandard
updateBoardWithFigureAtXY board px py Z RRight = updateBoardWithFigureAtXY board px py Z RLeft

updateBoardWithFigureAtXY board px py T RStandard =
  updateBoardAtXY (updateBoardAtXY (updateBoardAtXY (updateBoardAtXY board px py) (px + 1) py) (px - 1) py) px (py - 1)
updateBoardWithFigureAtXY board px py T RLeft =
  updateBoardAtXY (updateBoardAtXY (updateBoardAtXY (updateBoardAtXY board px py) px (py - 1)) px (py - 2)) (px - 1) (py - 1)
updateBoardWithFigureAtXY board px py T RInverted =
  updateBoardAtXY (updateBoardAtXY (updateBoardAtXY (updateBoardAtXY board px py) (px + 1) (py - 1)) (px - 1) (py - 1)) px (py - 1)
updateBoardWithFigureAtXY board px py T RRight =
  updateBoardAtXY (updateBoardAtXY (updateBoardAtXY (updateBoardAtXY board px py) px (py - 1)) px (py - 2)) (px + 1) (py - 1)

updateBoardWithFigureAtXY board px py L RStandard =
  updateBoardAtXY (updateBoardAtXY (updateBoardAtXY (updateBoardAtXY board px py) (px + 1) py) (px - 1) py) (px + 1) (py - 1)
updateBoardWithFigureAtXY board px py L RLeft =
  updateBoardAtXY (updateBoardAtXY (updateBoardAtXY (updateBoardAtXY board px py) px (py - 1)) px (py - 2)) (px - 1) (py - 2)
updateBoardWithFigureAtXY board px py L RInverted =
  updateBoardAtXY (updateBoardAtXY (updateBoardAtXY (updateBoardAtXY board (px-1) py) (px + 1) (py - 1)) (px - 1) (py - 1)) px (py - 1)
updateBoardWithFigureAtXY board px py L RRight =
  updateBoardAtXY (updateBoardAtXY (updateBoardAtXY (updateBoardAtXY board px py) (px + 1) py) px (py - 1)) px (py - 2)

updateBoardWithFigureAtXY board px py J RStandard =
  updateBoardAtXY (updateBoardAtXY (updateBoardAtXY (updateBoardAtXY board px py) (px + 1) py) (px - 1) py) (px - 1) (py - 1)
updateBoardWithFigureAtXY board px py J RLeft =
  updateBoardAtXY (updateBoardAtXY (updateBoardAtXY (updateBoardAtXY board px py) (px - 1) py) px (py - 1)) px (py - 2)
updateBoardWithFigureAtXY board px py J RInverted =
  updateBoardAtXY (updateBoardAtXY (updateBoardAtXY (updateBoardAtXY board (px+1) py) (px + 1) (py - 1)) (px - 1) (py - 1)) px (py - 1)
updateBoardWithFigureAtXY board px py J RRight =
  updateBoardAtXY (updateBoardAtXY (updateBoardAtXY (updateBoardAtXY board px py) px (py - 2)) px (py - 1)) (px + 1) (py - 2)
  
checkFigureOnBoard board p'x p'y O rotation =
  p'y > 21 ||
  p'x < 0 ||
  p'x > 8 ||
  (board !! p'y) !! p'x ||
  (board !! p'y) !! (p'x+1) ||
  (p'y > 0 && (board !! (p'y-1)) !! p'x) ||
  (p'y > 0 && (board !! (p'y-1)) !! (p'x+1))

checkFigureOnBoard board p'x p'y I RStandard =
  p'y > 21 ||
  p'x < 0 ||
  p'x > 9 ||
  (board !! p'y) !! p'x ||
  (p'y > 0 && (board !! (p'y-1)) !! p'x) ||
  (p'y > 1 && (board !! (p'y-2)) !! p'x) ||
  (p'y > 2 && (board !! (p'y-3)) !! p'x)
checkFigureOnBoard board p'x p'y I RInverted = checkFigureOnBoard board p'x p'y I RStandard
checkFigureOnBoard board p'x p'y I RLeft =
  p'y > 21 ||
  p'x < 2 ||
  p'x > 8 ||
  (board !! p'y) !! p'x ||
  ((board !! p'y) !! (p'x - 1)) ||
  ((board !! p'y) !! (p'x - 2)) ||
  ((board !! p'y) !! (p'x + 1))
checkFigureOnBoard board p'x p'y I RRight =
  p'y > 21 ||
  p'x < 1 ||
  p'x > 7 ||
  (board !! p'y) !! p'x ||
  ((board !! p'y) !! (p'x - 1)) ||
  ((board !! p'y) !! (p'x + 2)) ||
  ((board !! p'y) !! (p'x + 1))

checkFigureOnBoard board p'x p'y S RStandard =
  p'y > 21 ||
  p'x < 1 ||
  p'x > 8 ||
  (board !! p'y) !! p'x ||
  (board !! p'y) !! (p'x-1) ||
  (p'y > 1 && (board !! (p'y-1)) !! p'x) ||
  (p'y > 1 && (board !! (p'y-1)) !! (p'x+1))
checkFigureOnBoard board p'x p'y S RLeft =
  p'y > 21 ||
  p'x < 1 ||
  p'x > 9 ||
  (board !! p'y) !! p'x ||
  (p'y > 1 && (board !! (p'y-1)) !! p'x) ||
  (p'y > 1 && (board !! (p'y-1)) !! (p'x-1)) ||
  (p'y > 2 && (board !! (p'y-2)) !! (p'x-1))
checkFigureOnBoard board p'x p'y S RInverted = checkFigureOnBoard board p'x p'y S RStandard
checkFigureOnBoard board p'x p'y S RRight = checkFigureOnBoard board p'x p'y S RLeft

checkFigureOnBoard board p'x p'y Z RStandard =
  p'y > 21 ||
  p'x < 1 ||
  p'x > 8 ||
  (board !! p'y) !! p'x ||
  (board !! p'y) !! (p'x+1) ||
  (p'y > 1 && (board !! (p'y-1)) !! p'x) ||
  (p'y > 1 && (board !! (p'y-1)) !! (p'x-1))
checkFigureOnBoard board p'x p'y Z RLeft =
  p'y > 21 ||
  p'x < 0 ||
  p'x > 8 ||
  (board !! p'y) !! p'x ||
  (p'y > 1 && (board !! (p'y-1)) !! p'x) ||
  (p'y > 1 && (board !! (p'y-1)) !! (p'x+1)) ||
  (p'y > 2 && (board !! (p'y-2)) !! (p'x+1))
checkFigureOnBoard board p'x p'y Z RInverted = checkFigureOnBoard board p'x p'y Z RStandard
checkFigureOnBoard board p'x p'y Z RRight = checkFigureOnBoard board p'x p'y Z RLeft

checkFigureOnBoard board p'x p'y T RStandard =
  p'y > 21 ||
  p'x < 1 ||
  p'x > 8 ||
  (board !! p'y) !! p'x ||
  (board !! p'y) !! (p'x+1) ||
  (board !! p'y) !! (p'x-1) ||
  (p'y > 1 && (board !! (p'y-1)) !! p'x)
checkFigureOnBoard board p'x p'y T RLeft =
  p'y > 21 ||
  p'x < 1 ||
  p'x > 9 ||
  (board !! p'y) !! p'x ||
  (p'y > 1 && (board !! (p'y-1)) !! (p'x-1)) ||
  (p'y > 1 && (board !! (p'y-1)) !! p'x) ||
  (p'y > 2 && (board !! (p'y-2)) !! p'x)
checkFigureOnBoard board p'x p'y T RInverted = 
  p'y > 21 ||
  p'x < 1 ||
  p'x > 8 ||
  (board !! p'y) !! p'x ||
  (p'y > 1 && (board !! (p'y-1)) !! p'x) ||
  (p'y > 1 && (board !! (p'y-1)) !! (p'x-1)) ||
  (p'y > 1 && (board !! (p'y-1)) !! (p'x+1))
checkFigureOnBoard board p'x p'y T RRight = 
  p'y > 21 ||
  p'x < 0 ||
  p'x > 8 ||
  (board !! p'y) !! p'x ||
  (p'y > 1 && (board !! (p'y-1)) !! (p'x+1)) ||
  (p'y > 1 && (board !! (p'y-1)) !! p'x) ||
  (p'y > 2 && (board !! (p'y-2)) !! p'x)

checkFigureOnBoard board p'x p'y L RStandard =
  p'y > 21 ||
  p'x < 1 ||
  p'x > 8 ||
  (board !! p'y) !! p'x ||
  (board !! p'y) !! (p'x+1) ||
  (board !! p'y) !! (p'x-1) ||
  (p'y > 1 && (board !! (p'y-1)) !! (p'x + 1))
checkFigureOnBoard board p'x p'y L RLeft =
  p'y > 21 ||
  p'x < 1 ||
  p'x > 9 ||
  (board !! p'y) !! p'x ||
  (p'y > 2 && (board !! (p'y-2)) !! (p'x-1)) ||
  (p'y > 1 && (board !! (p'y-1)) !! p'x) ||
  (p'y > 2 && (board !! (p'y-2)) !! p'x)
checkFigureOnBoard board p'x p'y L RInverted = 
  p'y > 21 ||
  p'x < 1 ||
  p'x > 8 ||
  (board !! p'y) !! (p'x - 1) ||
  (p'y > 1 && (board !! (p'y-1)) !! p'x) ||
  (p'y > 1 && (board !! (p'y-1)) !! (p'x-1)) ||
  (p'y > 1 && (board !! (p'y-1)) !! (p'x+1))
checkFigureOnBoard board p'x p'y L RRight = 
  p'y > 21 ||
  p'x < 0 ||
  p'x > 8 ||
  (board !! p'y) !! p'x ||
  (p'y > 1 && (board !! (p'y-2)) !! p'x) ||
  (p'y > 1 && (board !! (p'y-1)) !! p'x) ||
  ((board !! p'y) !! (p'x+1))

checkFigureOnBoard board p'x p'y J RStandard =
  p'y > 21 ||
  p'x < 1 ||
  p'x > 8 ||
  (board !! p'y) !! p'x ||
  (board !! p'y) !! (p'x+1) ||
  (board !! p'y) !! (p'x-1) ||
  (p'y > 1 && (board !! (p'y-1)) !! (p'x-1))
checkFigureOnBoard board p'x p'y J RLeft =
  p'y > 21 ||
  p'x < 1 ||
  p'x > 9 ||
  (board !! p'y) !! p'x ||
  ((board !! p'y) !! (p'x-1)) ||
  (p'y > 1 && (board !! (p'y-1)) !! p'x) ||
  (p'y > 2 && (board !! (p'y-2)) !! p'x)
checkFigureOnBoard board p'x p'y J RInverted = 
  p'y > 21 ||
  p'x < 1 ||
  p'x > 8 ||
  (board !! p'y) !! (p'x+1) ||
  (p'y > 1 && (board !! (p'y-1)) !! p'x) ||
  (p'y > 1 && (board !! (p'y-1)) !! (p'x-1)) ||
  (p'y > 1 && (board !! (p'y-1)) !! (p'x+1))
checkFigureOnBoard board p'x p'y J RRight = 
  p'y > 21 ||
  p'x < 0 ||
  p'x > 8 ||
  (board !! p'y) !! p'x ||
  (p'y > 1 && (board !! (p'y-1)) !! p'x) ||
  (p'y > 2 && (board !! (p'y-2)) !! p'x) ||
  (p'y > 2 && (board !! (p'y-2)) !! (p'x+1))

clearTetrisAtBoard board points 22 = (board , points)
clearTetrisAtBoard board points y =
  if foldr (&&) True (board !! y)
  then clearTetrisAtBoard ([([0 .. 10] >>= \x -> [False])] ++ take y board ++ drop (y+1) board) (points + 10) (y + 1)
  else clearTetrisAtBoard (board) (points + 10) (y + 1)

appLoop :: Renderer -> GameState -> IO ()
appLoop renderer state =
  pollEvents >>= \events ->
  (randomIO :: IO Piece) >>= \ pieceR -> 
  let state' = foldr handleEvent state events in
  let (gameOver , p , board, downTimerMax, downTimer, piece, rotation) = evalState state' initState in
  let (p' , dtm , dt) = 
        if downTimer == 0
        then (p + V2 0 1 , downTimerMax , downTimerMax)
        else (p , downTimerMax , downTimer - 1)
  in
  let (p'' , board', piece', rotation' , points) =
        let (V2 px' py') = p in
        let (px , py) = ((fromInteger (toInteger px')) , (fromInteger (toInteger py'))) in
        let (V2 p'x' p'y') = p' in
        let (p'x , p'y) = ((fromInteger (toInteger p'x')) , (fromInteger (toInteger p'y'))) in
          if checkFigureOnBoard board p'x p'y piece rotation
          then
            let (board'' , points) = clearTetrisAtBoard (updateBoardWithFigureAtXY board px py piece rotation) 0 0 in
            (V2 4 0 , board'' , pieceR, RStandard , points)
          else (p' , board, piece, rotation, 0)
  in
  let gameOver' = gameOver || foldr (||) False (board' !! 0 ++ board' !! 1) in
  let state' = (gameOver' , p'' , board' , dtm , dt, piece', rotation')
  in
  let boardState = ([0 .. 9] >>= \x -> [0 .. 21] >>= \y ->
                       if ((board !! y) !! x)
                       then [posToSquare (V2 (fromInteger (toInteger x)) (fromInteger (toInteger y)))]
                       else [])
  in
    do      
      -- Clear render
      rendererDrawColor renderer $= V4 255 255 255 255
      clear renderer
    
      -- Draw everything

      -- Horizontal lines
      rendererDrawColor renderer $= V4 0 0 0 255
      sequence $ map (fillRect renderer . Just) ([0 .. 22] >>= \x -> [Rectangle (P (V2 0 (x * (2 + yStepSize)))) (V2 screenWidth 2)])

      -- Vertical lines
      sequence $ map (fillRect renderer . Just) ([0 .. 10] >>= \x -> [Rectangle (P (V2 (x * (2 + xStepSize)) 0)) (V2 2 screenHeight)])

      -- Piece position
      rendererDrawColor renderer $= V4 255 0 255 255
      sequence $ map (fillRect renderer . Just) boardState

      -- Player position
      renderType renderer p piece rotation
  
      -- Present
      present renderer
      threadDelay 1000
      (if gameOver
       then putStrLn "Exit"
       else appLoop renderer (return state'))
  
handleEvent :: Event -> GameState -> GameState

handleEvent (Event e QuitEvent) s =
  s >>= \(b,p,board,dtm,dt,piece, rotation) -> return (True,p,board,dtm,dt,piece, rotation)
handleEvent (Event e (KeyboardEvent (KeyboardEventData _ Pressed _ (Keysym ScancodeEscape _ _)))) s =
  s >>= \(b,p,board,dtm,dt,piece, rotation) -> return (True,p,board,dtm,dt,piece, rotation)

handleEvent (Event e (KeyboardEvent (KeyboardEventData _ Pressed False (Keysym ScancodeLeft _ _)))) s =
  s >>= \(b,(V2 x y),board,dtm,dt,piece, rotation) ->
  return (b, if checkFigureOnBoard board (fromInteger (toInteger x) - 1) (fromInteger (toInteger y)) piece rotation
             then (V2 x y)
             else (V2 x y) + V2 (-1) 0
         , board,dtm,dt,piece, rotation)
handleEvent (Event e (KeyboardEvent (KeyboardEventData _ Pressed False (Keysym ScancodeUp _ _)))) s =
  s >>= \(b,(V2 x y),board,dtm,dt,piece, rotation) ->
  let rotation' = case rotation of
                    RStandard -> RLeft
                    RLeft -> RInverted
                    RInverted -> RRight
                    RRight -> RStandard
  in
  return (b,(V2 x y),board,dtm,dt,piece,if checkFigureOnBoard board (fromInteger (toInteger x)) (fromInteger (toInteger y)) piece rotation'
                                        then rotation
                                        else rotation')

handleEvent (Event e (KeyboardEvent (KeyboardEventData _ Pressed False (Keysym ScancodeRight _ _)))) s =
  s >>= \(b,(V2 x y),board,dtm,dt,piece, rotation) ->
  return (b, if checkFigureOnBoard board (fromInteger (toInteger x) + 1) (fromInteger (toInteger y)) piece rotation
             then (V2 x y)
             else (V2 x y) + V2 1 0
         , board,dtm,dt,piece, rotation)
handleEvent (Event e (KeyboardEvent (KeyboardEventData _ Pressed False (Keysym ScancodeDown _ _)))) s =
  s >>= \(b,p,board,dtm,dt,piece, rotation) ->
  return (b,p,board,dtm,0,piece, rotation)


handleEvent (Event e (KeyboardEvent (KeyboardEventData _ Released False (Keysym ScancodeLeft _ _)))) s =
  s >>= \(b,p,board,dtm,dt,piece, rotation) -> return (b,p,board,dtm,dt,piece, rotation)
handleEvent (Event e (KeyboardEvent (KeyboardEventData _ Released False (Keysym ScancodeUp _ _)))) s =
  s >>= \(b,p,board,dtm,dt,piece, rotation) -> return (b,p,board,dtm,dt,piece, rotation)
handleEvent (Event e (KeyboardEvent (KeyboardEventData _ Released False (Keysym ScancodeRight _ _)))) s =
  s >>= \(b,p,board,dtm,dt,piece, rotation) -> return (b,p,board,dtm,dt,piece, rotation)
handleEvent (Event e (KeyboardEvent (KeyboardEventData _ Released False (Keysym ScancodeDown _ _)))) s =
  s >>= \(b,p,board,dtm,dt,piece, rotation) -> return (b,p,board,dtm,dt,piece, rotation)

handleEvent (Event e (KeyboardEvent (KeyboardEventData _ Pressed False (Keysym ScancodeSpace _ _)))) s =
  s >>= \(b,p,board,dtm,dt,piece, rotation) -> return (b,p,board,dtm,dt,piece, rotation)
handleEvent (Event e (KeyboardEvent (KeyboardEventData _ Pressed False (Keysym ScancodeE _ _)))) s = 
  s >>= \(b,p,board,dtm,dt,piece, rotation) -> return (b,p,board,dtm,dt,piece, rotation)
  
handleEvent (Event e _) s = s
