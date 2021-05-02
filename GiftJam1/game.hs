{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (unless)

import SDL hiding (get, E)
import SDL.Time 
import Linear (V4(..))
import Control.Concurrent (threadDelay)
import Foreign.C.Types

import Data.Text.Internal

import Data.Word

import Data.Bits

screenWidth = 1200
screenHeight = 900

playerSquareSize = 40
buttonSquareSize = 10

minDT = 0 -- 10000
gravity = 1
sideToSideSpeed = 0.5
jumpSpeedFactor = 1.5
  
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
  a + b = fromInteger ((dirToNum a) .|. (dirToNum b))
  a * b = fromInteger ((dirToNum a) .&. (dirToNum b)) -- TODO: Is this correct ? What is the semantics?

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

  fromInteger _ = O -- Error case

  negate a = fromInteger (xor 15 (dirToNum a))

  abs a = a -- TODO: Is this correct?
  signum a = a -- TODO: Is this correct?

data State = State { gameOver :: Bool
                   , characterNumber :: Int
                   , playerPos :: V2 CFloat
                   , playerDirection :: Direction
                   , deadPlayers :: [V2 CInt]
                   , currentTime :: Word32
                   , jump :: Bool
                   , jumpTimer :: CFloat
                   , currentLevel :: Int
                   , buttonsPressed :: [Int] }

data LevelObject = Box CInt CInt CInt CInt
                 | ButtonBox LevelObject [Int] LevelObject

data LevelButton = Button CInt CInt Int

data LevelEnemy = Spike CInt CInt CInt CInt

levelEnemies :: [[LevelEnemy]]
levelEnemies = [
  [],
  [Spike 300 (screenHeight - 50) 40 40,
   Spike 500 0 40 (screenHeight - 80),
   Spike 650 (screenHeight - 50) 40 40,
   Spike 700 (screenHeight - 80) 40 40]
  ]


levelButtons :: [[LevelButton]]
levelButtons = [
  [Button (screenWidth-50) (screenHeight-100) 0,
   Button 200 (screenHeight-200) 1],
  []
  ]

levels :: [[LevelObject]]
levels = [
  [Box 0 0 screenWidth 3,
   Box 0 0 3 screenHeight,
   ButtonBox (Box (screenWidth-3) 0 3 screenHeight) [0,1] (Box 0 0 0 0),
   Box 0 (screenHeight-3) screenWidth 3,
   ButtonBox (Box (200 - 20 + div buttonSquareSize 2) (screenHeight - 220 + div buttonSquareSize 2) 3 40) [0] (Box 0 0 0 0),
   ButtonBox (Box (200 - 20 + div buttonSquareSize 2) (screenHeight - 180 + div buttonSquareSize 2) 43 3) [0] (Box 0 0 0 0),
   ButtonBox (Box (200 + 20 + div buttonSquareSize 2) (screenHeight - 220 + div buttonSquareSize 2) 3 40) [0] (Box 0 0 0 0),
   ButtonBox (Box (200 - 20 + div buttonSquareSize 2) (screenHeight - 220 + div buttonSquareSize 2) 40 3) [0] (Box 0 0 0 0)],
  [Box 0 0 screenWidth 3,
   Box 0 0 3 screenHeight,
   Box 0 (screenHeight-3) screenWidth 3]
  ]
             

tupleToRectangle (x,y,w,h) = Rectangle (P (V2 x y)) (V2 w h)

v2FloatToInt :: V2 CFloat -> V2 CInt
v2FloatToInt (V2 a b) = V2 (toEnum . fromEnum $ a) (toEnum . fromEnum $ b)

v2IntToFloat :: V2 CInt -> V2 CFloat
v2IntToFloat (V2 a b) = V2 (toEnum . fromEnum $ a) (toEnum . fromEnum $ b)

initState :: State
initState = State { gameOver = False
                  , characterNumber = 1
                  , playerPos = V2 50 ((toEnum . fromEnum) screenHeight - 50)
                  , playerDirection = O
                  , deadPlayers = []
                  , currentTime = 4294967295
                  , jump = True
                  , jumpTimer = 150
                  , currentLevel = 0
                  , buttonsPressed = [] }

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
textOfNumber n | n `elem` [21..29] = mappend (mappend (textOfNumber 20) " ") (textOfNumber (n-20))
textOfNumber 30 = "Thirty"
textOfNumber n | n `elem` [31..39] = mappend (mappend (textOfNumber 30) " ") (textOfNumber (n-30))
textOfNumber 40 = "Fourty"
textOfNumber n | n `elem` [41..49] = mappend (mappend (textOfNumber 40) " ") (textOfNumber (n-40))
textOfNumber 50 = "Fifty"
textOfNumber n | n `elem` [51..59] = mappend (mappend (textOfNumber 50) " ") (textOfNumber (n-50))
textOfNumber 60 = "Sixty"
textOfNumber n | n `elem` [61..69] = mappend (mappend (textOfNumber 60) " ") (textOfNumber (n-60))
textOfNumber 70 = "Seventy"
textOfNumber n | n `elem` [71..79] = mappend (mappend (textOfNumber 70) " ") (textOfNumber (n-70))
textOfNumber 80 = "Eighty"
textOfNumber n | n `elem` [81..89] = mappend (mappend (textOfNumber 80) " ") (textOfNumber (n-80))
textOfNumber 90 = "Ninety"
textOfNumber n | n `elem` [91..99] = mappend (mappend (textOfNumber 90) " ") (textOfNumber (n-90))
textOfNumber n | n `elem` [100..999] = mappend (mappend (textOfNumber (div n 100)) " Hundred") (if mod n 100 == 0 then " " else mappend " and " (textOfNumber (mod n 100)))
textOfNumber n | n `elem` [1000..999999] = mappend (mappend (textOfNumber (div n 1000)) " Thousand") (if mod n 1000 == 0 then " " else mappend (if mod n 1000 < 100 then " and " else " ") (textOfNumber (mod n 1000)))
textOfNumber _ = "Alot"

velFromDirection :: Direction -> V2 CFloat
velFromDirection O = V2 0 0

velFromDirection N = V2 0 (-jumpSpeedFactor * gravity)
velFromDirection E = V2 sideToSideSpeed 0
velFromDirection S = V2 0 (jumpSpeedFactor * gravity)
velFromDirection W = V2 (-sideToSideSpeed) 0

velFromDirection NE = velFromDirection N + velFromDirection E
velFromDirection NW = velFromDirection N + velFromDirection W
velFromDirection SE = velFromDirection S + velFromDirection E
velFromDirection SW = velFromDirection S + velFromDirection W

levelObjectToV2Pair :: State -> LevelObject -> (V2 CFloat, V2 CFloat)
levelObjectToV2Pair _ (Box x y w h) = (v2IntToFloat $ V2 x y, v2IntToFloat $ V2 w h)
levelObjectToV2Pair s (ButtonBox lo0 c lo1) =
  levelObjectToV2Pair s $
  if foldr (\a b -> b && a `elem` buttonsPressed s) True c
  then lo1
  else lo0

rectangleFromLevelObject :: State -> LevelObject -> Rectangle CInt
rectangleFromLevelObject _ (Box x y w h) = Rectangle (P $ V2 x y) (V2 w h)
rectangleFromLevelObject s (ButtonBox lo0 c lo1) =
  rectangleFromLevelObject s $
  if foldr (\a b -> b && a `elem` buttonsPressed s) True c
  then lo1
  else lo0

projectV2 :: V2 a -> a
projectV2 (V2 x y) = x
  
appLoop :: Window -> Renderer -> State -> IO ()
appLoop window renderer state =
  pollEvents >>= \events ->
  return (foldr handleEvent state events) >>= \state ->
  ticks >>= \t1 ->
  let dt  = if currentTime state > 4294967294 then 10 else ((fromInteger . toInteger) (t1 - (currentTime state)) :: CFloat) in
  let initVel = velFromDirection (playerDirection state) + V2 0 gravity in
  let colBoxes = ((deadPlayers state) >>= \a -> return $ ((v2IntToFloat a), (v2IntToFloat $ V2 playerSquareSize playerSquareSize)))
              -- ++ [((v2IntToFloat $ V2 0 screenHeight),(v2IntToFloat $ V2 screenWidth 10))]
              ++ (levels !! currentLevel state >>= return . levelObjectToV2Pair state) in

  let (vel, (topHit, botHit,_)) = foldr (\(a,b) -> collisionCheck (playerPos state) (v2IntToFloat $ V2 playerSquareSize playerSquareSize) a b) (initVel, (False,False,False)) colBoxes in
  return (state { buttonsPressed = buttonsPressed state ++
                  (levelButtons !! currentLevel state >>= \(Button a b i) ->
                    let (_, (_,_,hit)) = collisionCheck (playerPos state) (v2IntToFloat $ V2 playerSquareSize playerSquareSize) (v2IntToFloat $ V2 a b) (v2IntToFloat $ V2 buttonSquareSize buttonSquareSize) (vel, (False,False,False)) in if hit then [i] else [])
                }) >>= \state ->

  return (if foldr (\(Spike x y w h) b -> let (_, (_,_,hit)) = collisionCheck (playerPos state) (v2IntToFloat $ V2 playerSquareSize playerSquareSize) (v2IntToFloat $ V2 x y) (v2IntToFloat $ V2 w h) (vel, (False,False,False)) in hit || b) False (levelEnemies !! currentLevel state)
          then killPlayer state
          else state ) >>= \state ->
    
  -- let (vel, (topHit, botHit)) =
  --       collisionCheck (playerPos state) (v2IntToFloat $ V2 playerSquareSize playerSquareSize) (v2IntToFloat $ V2 0 screenHeight) (v2IntToFloat $ V2 screenWidth 10) velResult in

  let preJump = jump state in
    
  return (state { playerPos = playerPos state + pure dt * vel
                , currentTime = t1
                , jump = jump state && not botHit
                , jumpTimer =
                    if jump state && not topHit
                    then jumpTimer state - dt
                    else 0
                }) >>= \state ->
  return (state { playerDirection =
                  if (not (jump state) && preJump) || (jumpTimer state < dt)
                  then (playerDirection state + N) + S
                  else playerDirection state
                }) >>= \state ->

  return (if (toEnum . fromEnum . projectV2) (playerPos state) > screenWidth
          then state { playerPos = playerPos initState
                      , currentLevel = currentLevel state + 1
                      , deadPlayers = []
                      , buttonsPressed = []
                      , gameOver = currentLevel state + 1 == length levels}
          else state) >>= \state ->

  unless (gameOver state)
  (do    
      windowTitle window $= mappend (textOfNumber (characterNumber state)) (" of many") -- 
      appFillRender renderer state
      -- Present
      present renderer

      -- delay (20000 - ((fromInteger . toInteger) dt))
      unless ((toEnum . fromEnum $ dt) > minDT) (threadDelay (minDT - ((toEnum . fromEnum) dt)))

      appLoop window renderer state)
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

    -- Draw player
    rendererDrawColor renderer $= V4 0 0 255 255
    (fillRect renderer . Just) (Rectangle (P (v2FloatToInt $ playerPos state)) (V2 playerSquareSize playerSquareSize))
  
    -- Draw dead players
    rendererDrawColor renderer $= V4 0 0 100 255
    sequence $ map (fillRect renderer . Just) (deadPlayers state >>= \x -> [Rectangle (P x) (V2 playerSquareSize playerSquareSize)])

    -- Draw level
    rendererDrawColor renderer $= V4 200 100 200 255
    sequence $ map (fillRect renderer . Just) (levels !! currentLevel state >>= return . rectangleFromLevelObject state)

    -- Draw buttons
    rendererDrawColor renderer $= V4 255 0 0 255
    sequence $ map (fillRect renderer . Just) (levelButtons !! currentLevel state >>= \(Button a b c) -> return $ Rectangle (P $ V2 a b) (V2 buttonSquareSize buttonSquareSize))

    -- Draw enemies
    rendererDrawColor renderer $= V4 255 255 0 255
    sequence $ map (fillRect renderer . Just) (levelEnemies !! currentLevel state >>= \(Spike x y w h) -> return $ Rectangle (P $ V2 x y) (V2 w h))


    return ()

minNorm a b = if a*a <= b * b
              then a
              else b

collisionCheck :: V2 CFloat -> V2 CFloat -> V2 CFloat -> V2 CFloat -> (V2 CFloat,(Bool,Bool,Bool)) -> (V2 CFloat,(Bool,Bool,Bool))
collisionCheck _ _ _ _ (V2 0 0, hit) = (V2 0 0, hit)
collisionCheck (V2 x y) (V2 wx wy) (V2 a b) (V2 la lb) (V2 vx vy, (topHit,botHit,hit)) =
  let rlux = a - (x+wx) in
  let rluy = b - (y+wy) in
  let rrdx = (a+la) - x in
  let rrdy = (b+lb) - y in
  let (xl,xr) = (min rlux rrdx, max rlux rrdx) in
  let (yl,yr) = (min rluy rrdy, max rluy rrdy) in
    if (xl < vx && vx < xr) && (yl < vy && vy < yr)
    then
      let xcz = minNorm rlux rrdx in
      let ycz = minNorm rluy rrdy in
        case (vx,vy) of
          (vx',0) -> (V2 xcz 0, (topHit,botHit,True)) -- Collision ?
          (0,vy') -> (V2 0 ycz, (vy <= 0,vy > 0,True)) -- Collision ?
          _ ->
            let tx = ((xcz*vx + ycz*vy) * vx) / (vx*vx + vy*vy) in
            let ty = ((xcz*vx + ycz*vy) * vy) / (vx*vx + vy*vy) in
            let cx = (xr + xl) / 2 in
            let cy = (yr + yl) / 2 in
            if ((xcz <= cx && ycz <= cy) && ((vx >  0 && ty <= ycz) || vx <= 0)) || -- bottom left  corner cases
               ((xcz >  cx && ycz <= cy) && ((vx <= 0 && ty <= ycz) || vx >  0)) || -- bottom right corner cases
               ((xcz <= cx && ycz  > cy) && ((vx >  0 && ty >  ycz) || vx <= 0)) || -- top    left  corner cases
               ((xcz >  cx && ycz  > cy) && ((vx <= 0 && ty >  ycz) || vx >  0))    -- top    right corner cases
            then (V2 vx ycz,(vy <= 0,vy > 0,True)) -- x free , y  hit
            else (V2 xcz vy,(topHit,botHit,True))   -- x  hit , y free
    else (V2 vx vy,(topHit,botHit,hit))

killPlayer :: State -> State
killPlayer s = s {playerPos = playerPos initState
                 , characterNumber = characterNumber s + 1
                 , deadPlayers = deadPlayers s ++ [v2FloatToInt $ playerPos s]
                 , jump = jump initState
                 , jumpTimer = jumpTimer initState}
  
handleEvent :: Event -> State -> State
handleEvent (Event e QuitEvent) s = s {gameOver = True}
handleEvent (Event e (KeyboardEvent (KeyboardEventData _ Pressed _ (Keysym sc kc km)))) s = handleKeyPressEvent (sc, kc, km) s
handleEvent (Event e (KeyboardEvent (KeyboardEventData _ Released _ (Keysym sc kc km)))) s = handleKeyReleaseEvent (sc, kc, km) s
handleEvent _ s = s

handleKeyPressEvent :: (Scancode, Keycode, KeyModifier) -> State -> State
handleKeyPressEvent (ScancodeEscape, _, _) s = s {gameOver = True}
handleKeyPressEvent (ScancodeLeft, _, _) s =   s {playerDirection = playerDirection s + W}
-- handleKeyPressEvent (ScancodeUp, _, _) s =     s {playerDirection = playerDirection s + N}
handleKeyPressEvent (ScancodeRight, _, _) s =  s {playerDirection = playerDirection s + E}
-- handleKeyPressEvent (ScancodeDown, _, _) s =   s {playerDirection = playerDirection s + S}
handleKeyPressEvent (ScancodeSpace, _, _) s =
  if not $ jump s
  then
    s {playerDirection = playerDirection s + N
         , jump = True
         , jumpTimer = jumpTimer initState }
  else s

handleKeyPressEvent (ScancodeDelete, _, _) s =
  if not $ jump s
  then killPlayer s
  else s

handleKeyPressEvent (_, _, _) s = s

handleKeyReleaseEvent :: (Scancode, Keycode, KeyModifier) -> State -> State
handleKeyReleaseEvent (ScancodeLeft, _, _) s =  s {playerDirection = playerDirection s - W}
-- handleKeyReleaseEvent (ScancodeUp, _, _) s =    s {playerDirection = playerDirection s - N}
handleKeyReleaseEvent (ScancodeRight, _, _) s = s {playerDirection = playerDirection s - E}
-- handleKeyReleaseEvent (ScancodeDown, _, _) s =  s {playerDirection = playerDirection s - S}
handleKeyReleaseEvent (_, _, _) s = s

