{-# LANGUAGE OverloadedStrings #-}
module Main where

import SDL hiding (get)
import SDL.Time 
import Linear (V4(..))
import Control.Monad (unless)
import Control.Monad.State
import Control.Concurrent
import Foreign.C.Types

gravity = V2 0 (-9)

playerVel = 10

playerWidth = 50
playerHeight = 50

groundHeight = 50

screenWidth = 1300
screenHeight = 900

jumpTimespan = 20

groundPlayerYPos = screenHeight - groundHeight - playerHeight
groundYPos = screenHeight - groundHeight

main :: IO ()
main = do
  initializeAll
  window <- createWindow "Ghost Wall" (defaultWindow {windowInitialSize = V2 screenWidth screenHeight})
  renderer <- createRenderer window (-1) defaultRenderer
  appLoop renderer (do a <- get
                       return a)

type Obsticle = Rectangle CInt
    
type InnerState = (V2 CInt, V2 CInt, V2 CInt, V2 CInt, Bool, (Bool, Int), [Obsticle])
type GameState = State InnerState InnerState

initState :: InnerState
initState = (V2 0 0, V2 0 0, gravity, V2 0 0,
             True,
             (False, 0),
             [Rectangle (P (V2 0 (- groundHeight))) (V2 screenWidth groundHeight),
              Rectangle (P (V2 100 60)) (V2 100 100),
              Rectangle (P (V2 300 0)) (V2 100 100)])

flipYAxis = (\(Rectangle (P (V2 x y)) (V2 w h)) -> Rectangle (P (V2 x (groundYPos - y))) (V2 w (-h)))

appLoop :: Renderer -> GameState -> IO ()
appLoop renderer state =
  do
    events <- pollEvents
    let state' = foldr handleEvent state events in
      let (p,v,a,cf,b,(jump,jumpTimer),obsticles) = evalState state' initState in
          let vel = v + a + cf + (if jumpTimer > 0 then V2 0 1 - gravity else V2 0 0) in
          let (vel',(topHit,botHit)) = foldr (collisionCheck p (V2 playerWidth playerHeight)) (vel,(False,False)) obsticles in
            let jumpTimer' = if jumpTimer > 0 && not botHit then jumpTimer - 1 else 0 in
          let cf' = case (cf - (vel - vel')) of V2 x' y' -> V2 x' 0 in
          let jump' =
                if jump
                then
                  if topHit
                  then False
                  else jumpTimer > 0
                else False in
            let p' = p + vel' in
              do
                -- Clear render
                rendererDrawColor renderer $= V4 255 255 255 255
                clear renderer 
  
                -- Draw everything
                rendererDrawColor renderer $= V4 255 255 0 255
                fillRect renderer (Just . flipYAxis $ Rectangle (P p') (V2 playerWidth playerHeight))
  
                -- rendererDrawColor renderer $= V4 255 0 255 255
                -- fillRect renderer (Just $ Rectangle (P (V2 0 (screenHeight - groundHeight))) (V2 screenWidth groundHeight))

                rendererDrawColor renderer $= V4 0 255 0 255
                sequence $ map (fillRect renderer . Just . flipYAxis) obsticles

                putStrLn . show $ (jump',topHit)
                putStrLn "\n"
  
                -- Present
                present renderer
                threadDelay 20000
                (if b
                 then appLoop renderer (return (p',vel'-cf',a,cf,b,(jump',jumpTimer'),obsticles))
                 else putStrLn "Exit")

minNorm a b = if a*a <= b * b
              then a
              else b

collisionCheck :: V2 CInt -> V2 CInt -> Obsticle -> (V2 CInt,(Bool,Bool)) -> (V2 CInt,(Bool,Bool))
collisionCheck _ _ _ (V2 0 0,hit) = (V2 0 0, hit)
collisionCheck (V2 x y) (V2 wx wy) (Rectangle (P (V2 a b)) (V2 la lb)) (V2 vx vy,(topHit,botHit)) =
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
          (vx',0) -> (V2 xcz 0, (topHit,botHit))
          (0,vy') -> (V2 0 ycz, (vy > 0,vy <= 0))
          _ ->
            let tx = div ((xcz*vx + ycz*vy) * vx) (vx*vx + vy*vy) in
            let ty = div ((xcz*vx + ycz*vy) * vy) (vx*vx + vy*vy) in
            let cx = div (xr + xl) 2 in
            let cy = div (yr + yl) 2 in
            if ((xcz <= cx && ycz <= cy) && ((vx >  0 && ty <= ycz) || vx <= 0)) || -- bottom left  corner cases
               ((xcz >  cx && ycz <= cy) && ((vx <= 0 && ty <= ycz) || vx >  0)) || -- bottom right corner cases
               ((xcz <= cx && ycz  > cy) && ((vx >  0 && ty >  ycz) || vx <= 0)) || -- top    left  corner cases
               ((xcz >  cx && ycz  > cy) && ((vx <= 0 && ty >  ycz) || vx >  0))    -- top    right corner cases
            then (V2 vx ycz,(vy > 0,vy <= 0)) -- x free , y  hit
            else (V2 xcz vy,(topHit,botHit))  -- x  hit , y free
    else (V2 vx vy,(topHit,botHit))
  
handleEvent :: Event -> GameState -> GameState

handleEvent (Event e QuitEvent) s = s >>= \(p,v,a,cf,_,jump,obsticle) -> return (p,v,a,cf,False,jump,obsticle)
handleEvent (Event e (KeyboardEvent (KeyboardEventData _ Pressed _ (Keysym ScancodeEscape _ _)))) s =
  s >>= \(p,v,a,cf,_,jump,obsticle) -> return (p,v,a,cf,False,jump,obsticle)

handleEvent (Event e (KeyboardEvent (KeyboardEventData _ Pressed False (Keysym ScancodeLeft _ _)))) s =
  s >>= \(p,v,a,cf,b,jump,obsticle) -> return (p,v,a,cf + V2 (-playerVel) 0,b,jump,obsticle)
-- handleEvent (Event e (KeyboardEvent (KeyboardEventData _ Pressed False (Keysym ScancodeUp _ _)))) s =
--   s >>= \(p,v,b,(left,_,right,down),jump,obsticle) -> return (p,v,b,jump,obsticle)
handleEvent (Event e (KeyboardEvent (KeyboardEventData _ Pressed False (Keysym ScancodeRight _ _)))) s =
  s >>= \(p,v,a,cf,b,jump,obsticle) -> return (p,v,a,cf + V2 playerVel 0,b,jump,obsticle)
-- handleEvent (Event e (KeyboardEvent (KeyboardEventData _ Pressed False (Keysym ScancodeDown _ _)))) s =
--   s >>= \(p,v,b,(left,up,right,_),jump,obsticle) -> return (p,v,b,jump,obsticle)

handleEvent (Event e (KeyboardEvent (KeyboardEventData _ Released False (Keysym ScancodeLeft _ _)))) s =
  s >>= \(p,v,a,cf,b,jump,obsticle) -> return (p,v,a,cf - V2 (-playerVel) 0,b,jump,obsticle)
-- handleEvent (Event e (KeyboardEvent (KeyboardEventData _ Released False (Keysym ScancodeUp _ _)))) s =
--   s >>= \(p,v,b,jump,obsticle) -> return (p,v-(V2 0 1),b,jump,obsticle)
handleEvent (Event e (KeyboardEvent (KeyboardEventData _ Released False (Keysym ScancodeRight _ _)))) s =
  s >>= \(p,v,a,cf,b,jump,obsticle) -> return (p,v,a,cf - V2 playerVel 0,b,jump,obsticle)
-- handleEvent (Event e (KeyboardEvent (KeyboardEventData _ Released False (Keysym ScancodeDown _ _)))) s =
--   s >>= \(p,v,b,jump,obsticle) -> return (p,v-(V2 0 1),b,jump,obsticle)

handleEvent (Event e (KeyboardEvent (KeyboardEventData _ Pressed False (Keysym ScancodeSpace _ _)))) s =
  s >>= \(p,v,b,a,cf,jump,obsticle) ->
          if fst jump
          then return (p,v,b,a,cf,jump,obsticle)
          else return (p,v,b,a,cf,(True,jumpTimespan),obsticle)

handleEvent (Event e _) s = s
