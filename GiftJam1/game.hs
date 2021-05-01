{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (unless)

import SDL hiding (get, E)
import SDL.Time 
import Linear (V4(..))
import Control.Concurrent
import Foreign.C.Types

import Data.Text.Internal

import Data.Word

screenWidth = 1200
screenHeight = 900

main :: IO ()
main = do
  initializeAll
  window <- createWindow "one of many" (defaultWindow {windowInitialSize = V2 screenWidth screenHeight})
  renderer <- createRenderer window (-1) defaultRenderer
  appLoop window renderer initState

data Direction = O | N | NE | E | SE | S | SW | W | NW

dirToNum :: Direction -> Integer
dirToNum O = 0
dirToNum N = 1
dirToNum E = 2
dirToNum S = 4
dirToNum W = 8
  
dirToNum NE = 3
dirToNum SE = 6
dirToNum NW = 9
dirToNum SW = 12

instance Num Direction where
  a + b = fromInteger (dirToNum a + dirToNum b)

  fromInteger 0 = O
  fromInteger 1 = N
  fromInteger 2 = E
  fromInteger 4 = S
  fromInteger 8 = W

  fromInteger 3 = NE
  fromInteger 6 = SE
  fromInteger 9 = NW
  fromInteger 12 = SW

  fromInteger 5 = O  -- NE
  fromInteger 7 = E  -- NSE
  fromInteger 10 = O -- EW
  fromInteger 11 = N -- NEW
  fromInteger 13 = W -- NSW
  fromInteger 14 = S -- SEW
  fromInteger 15 = O -- NSEW

  negate O = O
  negate N = S
  negate NE = SW
  negate E = W
  negate SE = NW
  negate S = N
  negate SW = NE
  negate W = E
  negate NW = SE

data State = State { gameOver :: Bool
                   , characterNumber :: Int
                   , playerPos :: V2 CInt
                   , playerDirection :: Direction
                   , deadPlayers :: [V2 CInt]
                   , currentTime :: Word32 }

tupleToRectangle (x,y,w,h) = Rectangle (P (V2 x y)) (V2 w h)

initState :: State
initState = State { gameOver = False
                  , characterNumber = 1
                  , playerPos = V2 50 50
                  , playerDirection = O
                  , deadPlayers = []
                  , currentTime = 0 }

textOfNumber :: Int -> Text
textOfNumber 0 = "Zero"
textOfNumber 1 = "One"
textOfNumber 2 = "Two"
textOfNumber 3 = "Three"
textOfNumber 4 = "Four"
textOfNumber 5 = "Five"
textOfNumber 6 = "Six"
textOfNumber 7 = "Seven"
textOfNumber 8 = "Eight"
textOfNumber 9 = "Nine"
textOfNumber 10 = "Ten"
textOfNumber 11 = "Eleven"
textOfNumber 12 = "Twelve"
textOfNumber 13 = "Thirteen"
textOfNumber 14 = "Fourteen"
textOfNumber 15 = "Fifteen"
textOfNumber 16 = "Sixteen"
textOfNumber 17 = "Seventeen"
textOfNumber 18 = "Eightteen"
textOfNumber 19 = "Nineteen"
textOfNumber 20 = "Twenty"
textOfNumber n | n `elem` [21..29] = mappend (textOfNumber 20) (textOfNumber (n-20))
textOfNumber _ = "Alot"

-- minDT = 20

velFromDirection :: Direction -> V2 CInt
velFromDirection O = V2 0 0

velFromDirection N = V2 0 (-1)
velFromDirection E = V2 1 0
velFromDirection S = V2 0 1
velFromDirection W = V2 (-1) 0

velFromDirection NE = velFromDirection N + velFromDirection E
velFromDirection NW = velFromDirection N + velFromDirection W
velFromDirection SE = velFromDirection S + velFromDirection E
velFromDirection SW = velFromDirection S + velFromDirection W

appLoop :: Window -> Renderer -> State -> IO ()
appLoop window renderer state =
  pollEvents >>= \events ->
  -- let state = (foldr handleEvent state events) in
  return (foldr handleEvent state events) >>= \state ->
  ticks >>= \t1 ->
  let dt  = ((fromInteger . toInteger) (t1 - (currentTime state)) :: CInt) in
  return (state { playerPos = playerPos state + pure dt * velFromDirection (playerDirection state)
                , currentTime = t1 }) >>= \state ->
    
  do
    -- putStrLn $ "DT: " ++ (show dt)
    
    windowTitle window $= mappend (textOfNumber (characterNumber state)) (" of many") -- 
    appFillRender renderer state
    -- Present
    present renderer

    -- delay (20000 - ((fromInteger . toInteger) dt))
    -- unless (((fromInteger . toInteger) dt) > minDT) (threadDelay (minDT - ((fromInteger . toInteger) dt)))

    unless (gameOver state) (appLoop window renderer state)
    -- (if gameOver state
    --  then putStrLn "Game Over"
    --  else appLoop window renderer state)

appFillRender :: Renderer -> State -> IO ()
appFillRender renderer state =
  do
    -- Clear render
    rendererDrawColor renderer $= V4 255 255 255 255
    clear renderer
    
    -- Draw everything

    rendererDrawColor renderer $= V4 0 0 255 255
    (fillRect renderer . Just) (Rectangle (P (playerPos state)) (V2 10 10))
  

    -- -- Horizontal lines
    -- rendererDrawColor renderer $= V4 0 0 0 255
    -- sequence $ map (fillRect renderer . Just) ([0 .. 22] >>= \x -> [Rectangle (P (V2 0 (x * (2 + yStepSize)))) (V2 screenWidth 2)])

    -- -- Vertical lines
    -- sequence $ map (fillRect renderer . Just) ([0 .. 10] >>= \x -> [Rectangle (P (V2 (x * (2 + xStepSize)) 0)) (V2 2 screenHeight)])

    return ()

  
handleEvent :: Event -> State -> State
handleEvent (Event e QuitEvent) s = s {gameOver = True}
handleEvent (Event e (KeyboardEvent (KeyboardEventData _ Pressed _ (Keysym sc kc km)))) s = handleKeyPressEvent (sc, kc, km) s
handleEvent (Event e (KeyboardEvent (KeyboardEventData _ Released _ (Keysym sc kc km)))) s = handleKeyReleaseEvent (sc, kc, km) s
handleEvent _ s = s

handleKeyPressEvent :: (Scancode, Keycode, KeyModifier) -> State -> State
handleKeyPressEvent (ScancodeEscape, _, _) s = s {gameOver = True}
handleKeyPressEvent (ScancodeLeft, _, _) s =   s {playerDirection = playerDirection s + W}
handleKeyPressEvent (ScancodeUp, _, _) s =     s {playerDirection = playerDirection s + N}
handleKeyPressEvent (ScancodeRight, _, _) s =  s {playerDirection = playerDirection s + E}
handleKeyPressEvent (ScancodeDown, _, _) s =   s {playerDirection = playerDirection s + S}
handleKeyPressEvent (_, _, _) s = s

handleKeyReleaseEvent :: (Scancode, Keycode, KeyModifier) -> State -> State
handleKeyReleaseEvent (ScancodeLeft, _, _) s =  s {playerDirection = playerDirection s - W}
handleKeyReleaseEvent (ScancodeUp, _, _) s =    s {playerDirection = playerDirection s - N}
handleKeyReleaseEvent (ScancodeRight, _, _) s = s {playerDirection = playerDirection s - E}
handleKeyReleaseEvent (ScancodeDown, _, _) s =  s {playerDirection = playerDirection s - S}
handleKeyReleaseEvent (_, _, _) s = s
