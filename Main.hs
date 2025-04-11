module Main where


import FourierSeries

import Data.Complex
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game (Event(EventMotion, EventKey), Key(MouseButton, SpecialKey, Char), SpecialKey(KeyEnter, KeyRight, KeyLeft), KeyState(Down, Up), MouseButton(LeftButton))




-- | Display values
timeScale :: Float
timeScale = 0.1     -- ^ determines the speed of the animation

trailLength :: Int
trailLength =  400  -- ^ reduce to have the trail of the FS fade off after time

width, height :: Int
width  = 500        -- ^ initial window width
height = 500        -- ^ initial window height

offset :: Int
offset =  50        -- ^ window offset from corner

background :: Color
background =  white -- ^ window background color

fps :: Int
fps =  30           -- ^ FPS the animation runs at

window :: Display
window =  InWindow "Fourier Display" (width, height) (offset, offset)


-- | Main program entry, runs game and assigns input handler
main :: IO ()
main = play window background fps initialState render inputHandler stepFunction


-- | Step function for every rendering step
--   appends drawn point and increases t by dt
stepFunction :: Float -> State -> State
stepFunction dt s | lmb s && not (comitted s) = s{arr = arr s ++ [x :+ y], t=t s + timeScale * dt}
                  | otherwise                 = s{t =t s + timeScale * dt}
    where
        (x,y) = mousePos s


-- | Rendering function
--   shows vectors, radii, trace of the FS, drawing and some text
--   depending of the current state 
render :: State -> Picture
render s = pictures [
    circles
  , drawing
  , trail
  , eachVectorsTip
  , eachVectorsArrow
  , text
    ]
    where
        drawing          = if showDraws s then color black . line . toArr . arr $ s else Blank
        trail            = color (makeColor 0.85 0.6 0.85 0.85) . line . toArr . map (\t' -> getEnd s{t=t'}) . getTrailTArr trailLength (t s) $ 0.0025
        eachVectorsTip   = if showLines s then pictures . map (\n -> color (dark blue) . pointAt . evaluateAfterN n $ s) . freqArr $ s else Blank
        eachVectorsArrow = if showLines s then pictures . map (`drawVector` s) . freqArr $ s else Blank
        circles          = if showCircs s then color (dark white) . pictures . map (\(n, c) -> translateC (getPrev n s) . circle . magnitude $ c) . filter (\(n, _) -> n /= 0) . zip (freqArr s) . coefficients $ s else Blank
        text             = translate (-200) (-200) . scale 0.1 0.1 . pictures $ [
                                Text "[c] toggle Circles   [LMB] draw   [Enter] calculate",
                                translate 0 (-200) . Text $ "[v] toggle Vectors   [<]/[>] change number of vectors: " ++ show (precision s),
                                translate 0 (-400) . Text $ "[d] toggle Drawing         (only before calc)"
                                ]
        
        -- | Interface for translate that takes Complex Float
        translateC :: Complex Float -> Picture -> Picture
        translateC (x :+ y) = translate x y


        -- | Returns the position of the sum of all previous vectors
        --   to the nth vector, evaluated at the current State
        getPrev :: Int -> State -> Complex Float
        getPrev n s = if n > 0 then
                        evaluateAfterN (-n+1) s
                    else
                        evaluateAfterN (-n) s

        -- | helper function that draws the vectors
        drawVector :: Int -> State -> Picture
        drawVector 0 s = line [(0,0), p']
            where
                p' = toTuple . evaluateAfterN 0 $ s
                toTuple (x :+ y) = (x,y)
        drawVector n s = line [p, p']
            where
                p  = toTuple (getPrev n s)
                p' = toTuple . evaluateAfterN n $ s

        -- | transforms a Complex into a tuple
        toTuple (x :+ y) = (x,y)

        -- | helper function that draws a point at a given complex location
        pointAt :: Complex Float -> Picture
        pointAt (x :+ y) = translate x y . circleSolid $ 1.3


-- | Returns the position of the sum of all vectors up to n at the current State
evaluateAfterN :: Int -> State -> Complex Float
evaluateAfterN m s@State {t=t, coefficients=cs} = sum [ c * cis (fromIntegral i * w * t) | (c,i) <- shrunkCs]
    where
        l = length cs
        n = ((l - 1) `div` 2) - abs m
        pos = if m <= 0 then 0 else 1
        period   = 1
        w        = 2 * pi / period
        shrunkCs = take (l - n * 2 - pos) . drop (n+pos) $ zip cs (freqArr s)


getEnd :: State -> Complex Float
getEnd s@State{coefficients=cs} = evaluateAfterN (-length cs) s


-- | Returns an array of n values, spaced by dt so that they are all >0
getTrailTArr :: Int -> Float -> Float -> [Float]
getTrailTArr n t dt | n >= ceiling (t / dt) = [0, dt .. t]
                    | otherwise             = [a, a+dt .. t]
    where
        a = t - fromIntegral n * dt


-- | Initial state of the drawing
initialState :: State
initialState = State [] [] False (0,0) 0 False True True True 20


-- | Input function for handling mouse inputs while drawing and button events
inputHandler :: Event -> State -> State
inputHandler (EventKey (MouseButton LeftButton) Down _ pos) s                    = s{lmb=True, mousePos=pos}
inputHandler (EventKey (MouseButton LeftButton) Up _ _)     s                    = s{lmb=False}
inputHandler (EventMotion pos)                              s                    = s{mousePos=pos}
inputHandler (EventKey (Char 'r') Down _ _)                 s                    = s{comitted=False, coefficients=[], arr=[], t=0}
inputHandler (EventKey (Char 'd') Down _ _)                 s                    = s{showDraws=not (showDraws s)}
inputHandler (EventKey (Char 'c') Down _ _)                 s                    = s{showCircs=not (showCircs s)}
inputHandler (EventKey (Char 'v') Down _ _)                 s                    = s{showLines=not (showLines s)}
inputHandler (EventKey (Char '1') Down _ _)                 s                    = s{arr=ex1}
inputHandler (EventKey (Char '2') Down _ _)                 s                    = s{arr=ex2}
inputHandler (EventKey (SpecialKey KeyLeft)  Down _ _)      s | not (comitted s) = s{precision=max (precision s - 1) 1}
inputHandler (EventKey (SpecialKey KeyRight) Down _ _)      s | not (comitted s) = s{precision=precision s + 1}
inputHandler (EventKey (SpecialKey KeyEnter) Down _ _)      s | not (comitted s) = s{comitted=True,
                                                                                     coefficients=getCoefficients s . toFunctionOfT . arr $ s,
                                                                                     t=0}
inputHandler _ s = s


-- | Transforms an array of complex numbers into an array of tuples
toArr :: [Complex a] -> [(a, a)]
toArr =  map (\(a :+ b) -> (a,b))


-- | Example drawings
ex1, ex2 :: [Complex Float]
ex1 = [ 12.5 * (16 * sin t ^ 3) :+ (12.5 * (13 * cos t - 5 * cos (2 * t) - 2 * cos (3 * t) - cos (4 * t)) + 50)
      | t <- [0, 0.1 .. 2 * pi]]
ex2 = [ (-30) :+ (-25), (-60) :+ (-100), (-100) :+ (-100), (-20) :+ 100, 20 :+ 100,  100 :+ (-100), 60 :+ (-100), 30 :+ (-25), (-30) :+ (-25)]