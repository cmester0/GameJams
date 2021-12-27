{-# LANGUAGE OverloadedStrings #-}
module Main where

import SDL hiding (get)
import SDL.Time 
import Linear (V4(..))
import Control.Monad (unless)
import Control.Monad.State
import Control.Concurrent
import Foreign.C.Types

gravity = V2 0 (-2)
jumpForce = V2 0 29

playerVel = 10

playerWidth = 40
playerHeight = 40

screenWidth = 1900
screenHeight = 1055

jumpTimespan = 1

groundPlayerYPos = screenHeight - playerHeight
groundYPos = screenHeight

main :: IO ()
main = do
  initializeAll
  window <- createWindow "Ghost Wall" (defaultWindow {windowInitialSize = V2 screenWidth screenHeight})
  renderer <- createRenderer window (-1) defaultRenderer
  appLoop renderer (do a <- get
                       return a)

type Obsticle = Rectangle CInt
              
type InnerState = (V2 CInt, V2 CInt, V2 CInt, V2 CInt, Bool, (Bool, Int), [[Obsticle]], Int)
type GameState = State InnerState InnerState

tupleToRectangle (x,y,w,h) = Rectangle (P (V2 x y)) (V2 w h)

initState :: InnerState
initState = (V2 10 44, V2 0 0, V2 0 0, V2 0 0,
             True,
             (False, 0),
             map (map tupleToRectangle) [[(380,-47,84,218),(639,-63,94,180),(950,-63,58,383),(1152,-63,112,463),(1044,127,62,22),(1356,307,34,383),(1233,121,341,59),(1533,174,40,200),(1378,367,67,33),(1595,485,167,21),(1699,-10,63,515),(798,158,34,32),(847,159,37,30),(106,134,48,29),(161,132,44,35),(220,140,42,16),(272,137,40,25),(326,142,45,22)],[]],
              0)

border = map tupleToRectangle [(0,34,screenWidth,10),(0,0,10,screenHeight),(0,screenHeight-10,screenWidth,10),(screenWidth-10,0,10,screenHeight)]

setLvl :: InnerState -> Int -> InnerState
setLvl (p,v,a,cf,b,jump,obsticles,_) lvl = (p,v,a,cf,b,jump,obsticles,lvl)

setConstantForce :: InnerState -> V2 CInt -> InnerState
setConstantForce (p,v,a,_,b,jump,obsticles,lvl) cf = (p,v,a,cf,b,jump,obsticles,lvl)

flipYAxis = (\(Rectangle (P (V2 x y)) (V2 w h)) -> Rectangle (P (V2 x (groundYPos - y))) (V2 w (-h)))

fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f a =
  let p = f a in
  if p == a
  then p
  else fixpoint f p

appLoop :: Renderer -> GameState -> IO ()
appLoop renderer state =
  pollEvents >>= \events ->
  let state' = foldr handleEvent state events in
  let (p,v,a,cf,b,(jump,jumpTimer),obsticles,lvl) = evalState state' initState in
  if lvl >= length obsticles
  then putStrLn "Done with all levels!"
  else
    let vel = v + a + cf + (if jumpTimer == jumpTimespan then jumpForce else V2 0 0) in
    let (vel',(topHit,botHit)) = fixpoint (\v -> foldr (collisionCheck p (V2 playerWidth playerHeight)) v (border ++ (obsticles !! lvl))) (vel,(False,False)) in
    let jumpTimer' = if jumpTimer > 0 && not botHit then jumpTimer - 1 else 0 in
    let cf' = cf - (vel - vel') in
    let jump' =
          if jump
          then
            if topHit
            then False
            else True
          else False in
    let p' = p + vel' in
    let state' =
          case p' of
            V2 x y ->
              if False
              then setConstantForce (setLvl initState (lvl+1)) cf
              else (p',vel'-cf',a,cf,b,(jump',jumpTimer'),obsticles,lvl)
    in
      do
        -- Clear render
        rendererDrawColor renderer $= V4 255 255 255 255
        clear renderer  
    
        -- Draw everything
        rendererDrawColor renderer $= V4 255 255 0 255
        fillRect renderer (Just . flipYAxis $ Rectangle (P p') (V2 playerWidth playerHeight))  
  
        rendererDrawColor renderer $= V4 0 255 0 255
        sequence $ map (fillRect renderer . Just . flipYAxis) (obsticles !! lvl)

        -- Border
        rendererDrawColor renderer $= V4 255 0 0 255
        sequence $ map (fillRect renderer . Just . flipYAxis) border
  
        -- Present
        present renderer
        threadDelay 20000
        (if b
         then appLoop renderer (return state')
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
          (0,vy') -> (V2 0 ycz, (vy <= 0,vy > 0))
          _ ->
            let tx = div ((xcz*vx + ycz*vy) * vx) (vx*vx + vy*vy) in
            let ty = div ((xcz*vx + ycz*vy) * vy) (vx*vx + vy*vy) in
            let cx = div (xr + xl) 2 in
            let cy = div (yr + yl) 2 in
            if ((xcz <= cx && ycz <= cy) && ((vx >  0 && ty <= ycz) || vx <= 0)) || -- bottom left  corner cases
               ((xcz >  cx && ycz <= cy) && ((vx <= 0 && ty <= ycz) || vx >  0)) || -- bottom right corner cases
               ((xcz <= cx && ycz  > cy) && ((vx >  0 && ty >  ycz) || vx <= 0)) || -- top    left  corner cases
               ((xcz >  cx && ycz  > cy) && ((vx <= 0 && ty >  ycz) || vx >  0))    -- top    right corner cases
            then (V2 vx ycz,(vy <= 0,vy > 0)) -- x free , y  hit
            else (V2 xcz vy,(topHit,botHit))  -- x  hit , y free
    else (V2 vx vy,(topHit,botHit))
  
handleEvent :: Event -> GameState -> GameState

handleEvent (Event e QuitEvent) s = s >>= \(p,v,a,cf,_,jump,obsticle,lvl) -> return (p,v,a,cf,False,jump,obsticle,lvl)
handleEvent (Event e (KeyboardEvent (KeyboardEventData _ Pressed _ (Keysym ScancodeEscape _ _)))) s =
  s >>= \(p,v,a,cf,_,jump,obsticle,lvl) -> return (p,v,a,cf,False,jump,obsticle,lvl)

handleEvent (Event e (KeyboardEvent (KeyboardEventData _ Pressed False (Keysym ScancodeLeft _ _)))) s =
  s >>= \(p,v,a,cf,b,jump,obsticle,lvl) -> return (p,v,a,cf + V2 (-playerVel) 0,b,jump,obsticle,lvl)
handleEvent (Event e (KeyboardEvent (KeyboardEventData _ Pressed False (Keysym ScancodeUp _ _)))) s =
  s >>= \(p,v,a,cf,b,jump,obsticle,lvl) -> return (p,v,a,cf + V2 0 playerVel,b,jump,obsticle,lvl)
handleEvent (Event e (KeyboardEvent (KeyboardEventData _ Pressed False (Keysym ScancodeRight _ _)))) s =
  s >>= \(p,v,a,cf,b,jump,obsticle,lvl) -> return (p,v,a,cf + V2 playerVel 0,b,jump,obsticle,lvl)
handleEvent (Event e (KeyboardEvent (KeyboardEventData _ Pressed False (Keysym ScancodeDown _ _)))) s =
  s >>= \(p,v,a,cf,b,jump,obsticle,lvl) -> return (p,v,a,cf + V2 0 (-playerVel),b,jump,obsticle,lvl)

handleEvent (Event e (KeyboardEvent (KeyboardEventData _ Released False (Keysym ScancodeLeft _ _)))) s =
  s >>= \(p,v,a,cf,b,jump,obsticle,lvl) -> return (p,v,a,cf - V2 (-playerVel) 0,b,jump,obsticle,lvl)
handleEvent (Event e (KeyboardEvent (KeyboardEventData _ Released False (Keysym ScancodeUp _ _)))) s =
  s >>= \(p,v,a,cf,b,jump,obsticle,lvl) -> return (p,v,a,cf - V2 0 playerVel,b,jump,obsticle,lvl)
handleEvent (Event e (KeyboardEvent (KeyboardEventData _ Released False (Keysym ScancodeRight _ _)))) s =
  s >>= \(p,v,a,cf,b,jump,obsticle,lvl) -> return (p,v,a,cf - V2 playerVel 0,b,jump,obsticle,lvl)
handleEvent (Event e (KeyboardEvent (KeyboardEventData _ Released False (Keysym ScancodeDown _ _)))) s =
  s >>= \(p,v,a,cf,b,jump,obsticle,lvl) -> return (p,v,a,cf - V2 0 (-playerVel),b,jump,obsticle,lvl)

handleEvent (Event e (KeyboardEvent (KeyboardEventData _ Pressed False (Keysym ScancodeSpace _ _)))) s =
  s >>= \(p,v,a,cf,b,jump,obsticle,lvl) ->
          if fst jump
          then return (p,v,a,cf,b,jump,obsticle,lvl)
          else return (p,v,a,cf,b,(True,jumpTimespan),obsticle,lvl)

handleEvent (Event e (KeyboardEvent (KeyboardEventData _ Pressed False (Keysym ScancodeE _ _)))) s = 
  s >>= \(p,v,a,cf,b,jump,obsticle,lvl) -> return $ setConstantForce (setLvl initState lvl) cf
  
handleEvent (Event e _) s = s
