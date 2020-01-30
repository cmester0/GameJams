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

groundHeight = 50

screenWidth = 1900
screenHeight = 1055

jumpTimespan = 1

groundPlayerYPos = screenHeight - groundHeight - playerHeight
groundYPos = screenHeight - groundHeight

goalXPos = screenWidth - 75

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

ground :: Obsticle
ground = Rectangle (P (V2 (-100) (-groundHeight-100))) (V2 (screenWidth+200) (groundHeight+100))

initState :: InnerState
initState = (V2 0 0, V2 0 0, gravity, V2 0 0,
             True,
             (False, 0),
              map (map (\((x,y,w,h)) -> Rectangle (P (V2 x y)) (V2 w h))) [[],[(100,0,50,100),(300,0,50,200),(350,0,50,100)],[(295,4,89,94),(460,205,113,81),(474,115,10,99),(407,58,138,103),(1247,-33,129,59),(1273,-33,103,81),(1313,21,58,277),(1336,191,103,161),(695,306,108,66),(879,321,104,61),(1078,288,101,86),(1246,259,103,49)],[(357,6,117,135),(569,155,143,35),(753,204,22,174),(619,273,32,40),(832,425,144,13),(1189,426,55,25),(1358,407,38,31),(1480,391,73,36),(1559,-11,75,395)],[(363,-18,49,58),(443,-47,52,126),(555,-17,75,144),(671,-13,96,208),(828,-16,76,281),(974,-21,119,405),(646,321,147,98),(873,378,132,154),(779,323,32,43),(625,-21,435,69),(954,22,43,108)],[(1631,-12,82,376),(1617,333,52,265),(1589,582,50,263),(1407,-20,73,150),(1381,-11,51,62),(1456,120,23,186),(1444,210,16,23),(928,373,457,99),(1376,355,32,33),(1094,471,34,92),(1222,629,84,48),(1185,757,319,35)],[(500,0,45,902),(495,0,55,112),(496,0,53,257),(497,0,51,439),(498,0,49,604),(499,0,47,797),(636,973,430,38),(1412,532,130,47),(1778,-7,50,262)],[(0,969,1909,47),(1655,0,76,218),(102,0,67,915),(46,109,62,38),(49,213,60,47),(41,349,64,48),(40,451,66,32),(47,534,66,33),(45,626,64,49),(43,742,63,32),(37,850,73,33),(1498,53,76,934),(158,855,1264,56),(249,741,1257,59),(131,649,1317,46),(227,546,1293,44),(142,449,1303,56),(220,328,1297,57),(139,200,1294,68),(228,106,1293,54)],[(228,-32,77,58),(290,-11,102,74),(384,-36,150,150),(510,-8,212,177),(710,-20,313,272),(978,-23,365,402),(1483,-63,124,582),(1076,453,180,88),(1237,446,136,38),(900,516,225,127),(729,590,198,130),(626,710,184,97),(466,803,202,60),(335,855,148,120),(570,987,1339,29),(1593,483,141,514)],[(1735,0,50,959),(69,58,1613,52),(30,72,38,924),(118,167,1630,73),(33,296,1656,88),(115,463,1618,44),(42,565,1636,53),(135,670,1618,79),(38,811,1645,71),(1732,963,58,47),(1738,972,43,24),(1728,967,53,37)],[(380,-7,94,280),(229,139,175,45),(0,259,129,39),(377,288,97,16),(379,323,89,16),(379,365,91,14),(380,273,90,16),(369,307,101,13),(384,347,85,15),(386,298,98,12),(396,312,78,34),(403,345,80,31),(371,414,156,4),(316,464,259,29),(623,96,72,457),(689,10,409,36),(930,-10,83,376),(848,319,191,211),(745,152,241,52),(653,373,121,17)],[(663,11,604,164),(856,152,232,380),(863,529,145,219),(863,482,205,286),(871,729,187,48),(885,758,157,36),(910,760,121,46),(932,787,64,30),(642,-3,650,147),(735,152,418,44),(955,845,17,59),(856,936,63,10),(739,899,19,37),(641,811,25,37),(534,696,54,66),(466,590,32,46),(364,473,52,54),(321,339,2,76),(291,323,89,90),(696,295,58,69),(1056,883,130,57),(1275,766,65,66),(1448,517,34,114),(1416,307,53,101),(1594,144,88,74),(1165,502,60,75),(172,200,54,43),(111,69,71,62),(57,-12,102,57),(629,788,65,29),(724,878,62,23),(628,-3,674,82),(652,104,636,53),(625,3,624,97),(1277,24,30,78),(608,27,48,86),(1274,75,38,38)],[(179,91,11,32),(262,148,12,30),(361,201,13,40),(465,276,15,53),(569,329,13,52),(656,391,12,58),(793,410,15,65),(930,447,11,72),(1029,-4,80,532)],[(96,79,26,60),(101,234,22,64),(98,367,28,67),(95,504,24,56),(93,630,22,53),(174,734,19,59),(308,830,14,52),(468,834,8,65),(610,836,12,72),(733,867,10,10),(849,846,12,11),(1316,-15,81,519),(1320,584,74,432),(1139,408,12,19),(866,148,10,13),(1004,251,9,11),(453,824,22,79)],[(371,-18,51,153),(471,117,54,78),(543,144,214,94),(816,206,45,207),(735,280,52,34),(876,-63,1,643),(872,-63,1,556),(887,-55,15,695)],[(399,759,457,14),(981,762,595,15),(879,606,15,97),(844,464,25,116),(795,322,40,126),(865,316,88,21),(943,349,67,22),(1011,318,52,19),(952,-8,18,317),(539,591,26,119),(652,379,18,82),(541,187,21,125),(413,-20,19,85),(470,81,19,62),(500,184,18,78),(538,374,27,97),(608,313,16,73),(685,143,13,84),(631,549,16,101),(1051,342,0,62),(1052,339,13,54),(1176,532,14,149),(1350,680,11,86),(1297,358,22,163),(1377,147,20,129),(715,262,0,72),(714,446,0,102),(718,434,12,60),(726,240,16,84),(1231,451,10,212),(1453,402,10,135),(1191,179,12,143),(1276,59,18,104),(1459,217,12,84),(963,213,17,90),(950,291,54,22),(994,273,0,27),(991,132,6,169)],[(380,-47,84,218),(639,-63,94,180),(950,-63,58,383),(1152,-63,112,463),(1044,127,62,22),(1356,307,34,383),(1233,121,341,59),(1533,174,40,200),(1378,367,67,33),(1595,485,167,21),(1699,-10,63,515),(798,158,34,32),(847,159,37,30),(106,134,48,29),(161,132,44,35),(220,140,42,16),(272,137,40,25),(326,142,45,22)],[]],
              0)

setLvl :: InnerState -> Int -> InnerState
setLvl (p,v,a,cf,b,jump,obsticles,_) lvl = (p,v,a,cf,b,jump,obsticles,lvl)

setConstantForce :: InnerState -> V2 CInt -> InnerState
setConstantForce (p,v,a,_,b,jump,obsticles,lvl) cf = (p,v,a,cf,b,jump,obsticles,lvl)

flipYAxis = (\(Rectangle (P (V2 x y)) (V2 w h)) -> Rectangle (P (V2 x (groundYPos - y))) (V2 w (-h)))

appLoop :: Renderer -> GameState -> IO ()
appLoop renderer state =
  pollEvents >>= \events ->
  let state' = foldr handleEvent state events in
  let (p,v,a,cf,b,(jump,jumpTimer),obsticles,lvl) = evalState state' initState in
  if lvl >= length obsticles
  then putStrLn "Done with all levels!"
  else
    let vel = v + a + cf + (if jumpTimer == jumpTimespan then jumpForce else V2 0 0) in
    let (vel',(topHit,botHit)) = foldr (collisionCheck p (V2 playerWidth playerHeight)) (vel,(False,False)) (ground : (obsticles !! lvl)) in
    let jumpTimer' = if jumpTimer > 0 && not botHit then jumpTimer - 1 else 0 in
    let cf' = case (cf - (vel - vel')) of V2 x' y' -> V2 x' 0 in
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
            V2 x _ ->
              if x + playerWidth >= goalXPos
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
  
        rendererDrawColor renderer $= V4 255 0 255 255
        fillRect renderer (Just . flipYAxis $ ground)
  
        rendererDrawColor renderer $= V4 0 0 255 255
        fillRect renderer (Just $ Rectangle (P (V2 goalXPos 0)) (V2 playerWidth screenHeight  ))
  
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
            else (V2 xcz vy,(topHit,botHit))   -- x  hit , y free
    else (V2 vx vy,(topHit,botHit))
  
handleEvent :: Event -> GameState -> GameState

handleEvent (Event e QuitEvent) s = s >>= \(p,v,a,cf,_,jump,obsticle,lvl) -> return (p,v,a,cf,False,jump,obsticle,lvl)
handleEvent (Event e (KeyboardEvent (KeyboardEventData _ Pressed _ (Keysym ScancodeEscape _ _)))) s =
  s >>= \(p,v,a,cf,_,jump,obsticle,lvl) -> return (p,v,a,cf,False,jump,obsticle,lvl)

handleEvent (Event e (KeyboardEvent (KeyboardEventData _ Pressed False (Keysym ScancodeLeft _ _)))) s =
  s >>= \(p,v,a,cf,b,jump,obsticle,lvl) -> return (p,v,a,cf + V2 (-playerVel) 0,b,jump,obsticle,lvl)
-- handleEvent (Event e (KeyboardEvent (KeyboardEventData _ Pressed False (Keysym ScancodeUp _ _)))) s =
--   s >>= \(p,v,b,(left,_,right,down),jump,obsticle) -> return (p,v,b,jump,obsticle)
handleEvent (Event e (KeyboardEvent (KeyboardEventData _ Pressed False (Keysym ScancodeRight _ _)))) s =
  s >>= \(p,v,a,cf,b,jump,obsticle,lvl) -> return (p,v,a,cf + V2 playerVel 0,b,jump,obsticle,lvl)
-- handleEvent (Event e (KeyboardEvent (KeyboardEventData _ Pressed False (Keysym ScancodeDown _ _)))) s =
--   s >>= \(p,v,b,(left,up,right,_),jump,obsticle) -> return (p,v,b,jump,obsticle)

handleEvent (Event e (KeyboardEvent (KeyboardEventData _ Released False (Keysym ScancodeLeft _ _)))) s =
  s >>= \(p,v,a,cf,b,jump,obsticle,lvl) -> return (p,v,a,cf - V2 (-playerVel) 0,b,jump,obsticle,lvl)
-- handleEvent (Event e (KeyboardEvent (KeyboardEventData _ Released False (Keysym ScancodeUp _ _)))) s =
--   s >>= \(p,v,b,jump,obsticle) -> return (p,v-(V2 0 1),b,jump,obsticle)
handleEvent (Event e (KeyboardEvent (KeyboardEventData _ Released False (Keysym ScancodeRight _ _)))) s =
  s >>= \(p,v,a,cf,b,jump,obsticle,lvl) -> return (p,v,a,cf - V2 playerVel 0,b,jump,obsticle,lvl)
-- handleEvent (Event e (KeyboardEvent (KeyboardEventData _ Released False (Keysym ScancodeDown _ _)))) s =
--   s >>= \(p,v,b,jump,obsticle) -> return (p,v-(V2 0 1),b,jump,obsticle)

handleEvent (Event e (KeyboardEvent (KeyboardEventData _ Pressed False (Keysym ScancodeSpace _ _)))) s =
  s >>= \(p,v,a,cf,b,jump,obsticle,lvl) ->
          if fst jump
          then return (p,v,a,cf,b,jump,obsticle,lvl)
          else return (p,v,a,cf,b,(True,jumpTimespan),obsticle,lvl)

handleEvent (Event e (KeyboardEvent (KeyboardEventData _ Pressed False (Keysym ScancodeE _ _)))) s = 
  s >>= \(p,v,a,cf,b,jump,obsticle,lvl) -> return $ setConstantForce (setLvl initState lvl) cf
  
handleEvent (Event e _) s = s
