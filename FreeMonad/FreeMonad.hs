import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
-- data Card = Card { damage :: Int , health :: Int }

-- Snake
data Direction = U | L | D | R
data Snake = Snake Float [(Point, [(Float, Direction)])]
data World = World {players :: [Snake]}


speed = 20
screenWidth = 200
screenHeight = 200

------------
-- Render --
------------

renderSnake :: Snake -> [Picture]
renderSnake (Snake l []) = []
renderSnake (Snake l ((h, t) : rest)) =
  let ap = accumPoints h t in
    line (h : ap) : renderSnake (Snake l rest)

render :: World -> Picture
render w = pictures $ map (pictures . renderSnake) (players w)

----------------------
-- Helper functions --
----------------------

accumPoints :: Point -> [(Float, Direction)] -> [Point]
accumPoints _ [] = []
accumPoints p ((f, d) : t) =
  let x = updatePoint f (opposite d) p in
    x : accumPoints x t

opposite :: Direction -> Direction
opposite R = L
opposite L = R
opposite U = D
opposite D = U

updatePoint :: Float -> Direction -> Point -> Point
updatePoint dt U (x, y) = (x, y + speed * dt)
updatePoint dt L (x, y) = (x - speed * dt, y)
updatePoint dt D (x, y) = (x, y - speed * dt)
updatePoint dt R (x, y) = (x + speed * dt, y)

accumLimitedHelper :: Float -> Float -> [(Float, Direction)] -> (Float, [(Float, Direction)])
accumLimitedHelper _ a [] = (a, [])
accumLimitedHelper l a ((f, d) : x) =
  if a + f < l
  then
    let (r, t) = accumLimitedHelper l (a+f) x in
      (r, (f, d) : t)
  else
    if a < l
    then
      let (r, t) = accumLimitedHelper l l x in
        (r, (l - a, d) : t)
    else
      accumLimitedHelper l (a+f) x

accumLimited :: Float -> Float -> [(Point, [(Float, Direction)])] -> [(Point, [(Float, Direction)])]
accumLimited _ _ [] = []
accumLimited l a ((p, x) : rest) =
  let (r,t) = accumLimitedHelper l a x in
    (p, t) : accumLimited l r rest


floatRem :: Float -> Float -> Float
floatRem a b =
  if a < 0
  then floatRem (a + b) b
  else 
    if a > b
    then floatRem (a - b) b
    else a

------------
-- Update --
------------

updateSnake :: Float -> Snake -> Snake
updateSnake dt (Snake l ((h, (c,d) : t) : rest)) =
  let uc = min (c + dt) l in
  let newt = accumLimited l 0 ((h, (c,d) : t) : rest) in
  let (upx, upy) = updatePoint dt d h in
  let ut = 
        if (2 * upx < - screenWidth || 2 * upx > screenWidth ||
            2 * upy < - screenHeight || 2 * upy > screenHeight)
        then (((floatRem (upx + screenWidth / 2) (screenWidth) - screenWidth / 2,
                floatRem (upy + screenHeight / 2) (screenHeight) - screenHeight / 2),
               [(0,d),(uc, d)]) : newt)
        else ((upx, upy), (uc, d) : tail (snd (head newt))) : tail newt
  in
    Snake l ut

update :: Float -> World -> World
update dt w = w { players = map (updateSnake dt) (players w) }

------------
-- Events --
------------

eventDirection :: Direction -> Snake -> Snake
eventDirection dir (Snake l ((h, (c,d) : t) : rest)) = Snake l ((h, (0,dir) : (c,d) : t) : rest)

player0SnakeEvent :: Event -> Snake -> Snake
player0SnakeEvent (EventKey (SpecialKey KeyRight) _ _ _) w = eventDirection R w
player0SnakeEvent (EventKey (SpecialKey KeyLeft) _ _ _)  w = eventDirection L w
player0SnakeEvent (EventKey (SpecialKey KeyUp) _ _ _)    w = eventDirection U w
player0SnakeEvent (EventKey (SpecialKey KeyDown) _ _ _)  w = eventDirection D w
player0SnakeEvent (EventKey (SpecialKey KeySpace) _ _ _) (Snake l t) =
  Snake (l + 1 / speed) t
player0SnakeEvent _ x = x

event :: Event -> World -> World
-- event _ x = x
event e w = w {players = player0SnakeEvent e (head (players w)) : tail (players w)}

---------------
-- Game Loop --
---------------
  
main =
  play
  (InWindow "A Window" (round screenWidth, round screenHeight) (10, 10))
  white
  60
  (World {players = [Snake 1 [((0, 0), [(1, R)])]]})
  render -- Render
  event -- Event handler
  update -- Update
