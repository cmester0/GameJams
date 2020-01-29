import Graphics.UI.GLUT

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "Hello World"
  displayCallback $= display
  keyboardMouseCallback $= Just keyboardMouse
  mainLoop

myPoints :: [(GLfloat,GLfloat,GLfloat)]
myPoints =
  let m = 120 in
  [(sin (2*pi*k/m), cos (2*pi*k/m), 0) | k <- [1..m]]

color3f r g b = color $ Color3 r g (b :: GLfloat)
vertex3f x y z = vertex $ Vertex3 x y (z :: GLfloat)

keyboardMouse :: KeyboardMouseCallback
keyboardMouse (SpecialKey KeyLeft) Down _ (Position x y) = putStrLn "Left Down"
keyboardMouse (SpecialKey KeyLeft) Up _ (Position x y) = putStrLn "Left Up"
keyboardMouse _ _ _ (Position x y) = putStrLn "Nothing"  
  
display :: DisplayCallback
display = do
  clear [ ColorBuffer ]
  renderPrimitive Quads $ do
    color3f 1 0 0
    vertex3f 0 0 0
    vertex3f 0 0.2 0
    vertex3f 0.2 0.2 0
    vertex3f 0.2 0 0

    color3f 0 1 0
    vertex3f 0 0 0
    vertex3f 0 (-0.2) 0
    vertex3f 0.2 (-0.2) 0
    vertex3f 0.2 0 0

    color3f 0 0 1
    vertex3f 0 0 0
    vertex3f 0 (-0.2) 0
    vertex3f (-0.2) (-0.2) 0
    vertex3f (-0.2) 0 0

    color3f 1 0 1
    vertex3f 0 0 0
    vertex3f 0 0.2 0
    vertex3f (-0.2) 0.2 0
    vertex3f (-0.2) 0 0
  flush
