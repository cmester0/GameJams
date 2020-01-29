import Codec.Picture  (generateImage, writePng) -- JuicyPixels
import Codec.Picture.Types

width, height :: Integer
(width, height) = (2000, 2000)

max_iteration :: Integer
max_iteration = 500

iterFunction :: (Double,Double) -> (Double -> Double -> Bool) -> (Double -> Double -> Double) -> (Double -> Double -> Double) -> Integer -> Integer -> Integer
iterFunction (z0,z1) cond g1 g2 iter maxIter =
  if  cond z0 z1 && iter < maxIter
  then iterFunction (g1 z0 z1, g2 z0 z1) cond g1 g2  (iter+1) maxIter
  else iter

mandelbrot :: (Double,Double) -> (Double,Double) -> Integer -> Integer -> Integer
mandelbrot (c0,c1) (z0,z1) =
  iterFunction (z0,z1) (\z0 z1 -> z0^2 + z1^2 < 2^2) (\z0 z1 -> z0^2 - z1^2+c0) (\z0 z1 -> 2*z0*z1+c1)

angle_conversion h c x | 0 <= h && h < 60 = (c,x,0)
angle_conversion h c x | 60 <= h && h < 120 = (x,c,0)
angle_conversion h c x | 120 <= h && h < 180 = (0,c,x)
angle_conversion h c x | 180 <= h && h < 240 = (0,x,c)
angle_conversion h c x | 240 <= h && h < 300 = (x,0,c)
angle_conversion h c x | 300 <= h && h < 360 = (c,0,x)
angle_conversion h c x | 360 <= h = angle_conversion (h - 360) c x
angle_conversion h c x | h < 0 = angle_conversion (h + 360) c x

mod' x n | x >= n = mod' (x - n) n
mod' x n | x < 0 = x + n
mod' x _ = x

color_from_number :: Integer -> Integer -> (Integer,Integer,Integer)
color_from_number iter maxIter =
  let (h,s,l) = ((fromIntegral ((maxIter - iter) * 360)) / (fromIntegral maxIter),
                 1.0,
                 if h == 0 then 0 else 0.5) :: (Double,Double,Double) in
  let c = (1 - abs (2*l - 1)) * s in
  let x = c * (1 - abs ((mod' (h / 60) 2) - 1)) in
  let m = l - c / 2 in    
  let (r',g',b') = angle_conversion h c x in
    (round $ (r'+m)*255,
     round $ (g'+m)*255,
     round $ (b'+m)*255)

cx,cy :: Double
(cx,cy) = (-0.7490999, 0.099999999995)

zoom :: Double
zoom = 0.0000000002

pixelRenderer :: Int -> Int -> PixelRGB8
pixelRenderer x y =
  let (rx0,rx1) = (cx-zoom,cx+zoom) in
  let (ry0,ry1) = (cy-zoom,cy+zoom) in    
  let iter = mandelbrot ((fromIntegral x) / (fromIntegral width) * (rx1 - rx0) + rx0,
                         (fromIntegral y) / (fromIntegral height) * (ry1 - ry0) + ry0)
             (0,0) 0 max_iteration in
  let (r,g,b) = color_from_number iter max_iteration in
    PixelRGB8 (fromIntegral r) (fromIntegral g) (fromIntegral b)

renderTokens name = writePng name $ generateImage pixelRenderer (fromInteger width) (fromInteger height)

-- main = putStrLn . show $ color_from_number 0 360
main = renderTokens ("out_" ++ show width ++ "-" ++ show height ++ "_" ++ show max_iteration ++ "at_" ++ show cx ++ "_" ++ show cy ++ "_" ++ show zoom ++ ".png")
