module FourierSeries where


import Data.Complex


type Function = Float -> Complex Float


-- | Represents the state of the game system
data State = State
    {
        arr          :: [Complex Float] -- ^ array of drawn points
      , coefficients :: [Complex Float] -- ^ coefficients of the Fourier Transformation
      , lmb          ::  Bool           
      , mousePos     :: (Float, Float)
      , t            ::  Float          -- ^ the time parameter of the animation
      , comitted     ::  Bool           -- ^ indicates drawing vs. displaying state
      , showDraws    ::  Bool
      , showCircs    ::  Bool
      , showLines    ::  Bool
      , precision    ::  Int            -- ^ = number of vector pairs making up the Fourier Series
    }


-- | Returns the frequency assigned to each coefficient (in order)
freqArr :: State -> [Int]
freqArr s = [-precision s .. precision s]


-- | Calculates the coefficient for the complex trig function with frequency n given a function f
coefficient :: Int -> Function -> Complex Float
coefficient n f = (1/t :+ 0) * integral 0 1 func 1000
    where
        t = 1
        w = 2*pi/t
        func :: Float -> Complex Float
        func t = f t * cis (- fromIntegral n * w * t)


-- | Calculates all of the components necessary
getCoefficients :: State -> Function -> [Complex Float]
getCoefficients s f = map (`coefficient` f) (freqArr s)


-- | Approximates the integral by numeric methods
integral :: Float -> Float -> Function -> Int -> Complex Float
integral lower upper f steps = sum [f t | t <- [lower, lower + (dist/fromIntegral steps) .. upper]] * (dist/fromIntegral steps:+0)
    where
        dist :: Float
        dist = upper - lower


-- | Returns a parametric function that linearly interpolates between
--   two adjacent points in the input array (and normalises the speed)
toFunctionOfT :: [Complex Float] -> Function
toFunctionOfT list = \t -> sum [ c * (lerp (normaliseT list t) i :+ 0) | (c,i) <- zip list [0..]]
    where
        step :: Float
        step = 1.0/ fromIntegral (length list)

        lerp :: Float -> Int -> Float
        lerp t i = max 0 (1 - abs (fromIntegral i - t / step))


-- | Takes a list of points and value t and returns a parameter t' that is 
--   normalised in regards to the distance between each two points
normaliseT :: [Complex Float] -> Float -> Float
normaliseT list t | t == 1 = 1
                  | otherwise = tPrev + remap (t * sum lengths) (sum . take n $ lengths) (sum . take (n+1) $ lengths) 0 1 / segs
    where
        segs :: Float
        segs = fromIntegral (length lengths)
        n = getN t 0
        tPrev :: Float
        tPrev = fromIntegral n / segs

        getN t n | t * sum lengths <= sum (take (n+1) lengths) = n
                 | otherwise                               = getN t (n+1)

        lengths :: [Float]
        lengths = zipWith (\c1 c2 -> magnitude $ c2 - c1) list (tail list)


-- | Remaps the first value from min max to toMin toMax (extrapolating)
remap :: Float -> Float -> Float -> Float -> Float -> Float
remap x min max toMin toMax = toMin + (x - min) * (toMax - toMin) / (max - min)
