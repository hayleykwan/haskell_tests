import Data.List

data Colour = R | B | G deriving (Eq, Ord, Show)

type Colours = [Colour]

type Score = (Int, Int)

type Result = (Colours, Score)

colour :: Int -> Colour
colour n
   | n == 0 = R
   | n == 1 = B
   | n == 2 = G
   | otherwise = error "not correct input"

base3 :: Int -> Int -> [Int]
base3 x 0 = []
base3 x n = (base3 (x `div` 3) (n-1)) ++ [x `mod` 3]

blacks :: Colours -> Colours -> Int
blacks guess secret
  = length (filter isTrue (zipWith (==) guess secret))
  where isTrue x = x == True

score :: Colours -> Colours -> Result
score guess secret = (guess, (black, white))
  where
    white = length secret - length (guess \\ secret) - black
    black = blacks guess secret

allGuesses :: Int -> [Colours]
allGuesses n = map f [0..3^n-1]
  where f num = map colour (base3 num n)
-- allGuesses n = [ combinations | x <- [R,B,G], length combinations = n]

consistent :: Colours -> [Result] -> Bool
consistent guess results
  = (map rescore results) == results
    -- (s == newS) : (consistent guess scores)
    where
      rescore (prev, s) = score prev guess
      -- returning (prev, newscore)

strike :: [Colours] -> [Result] -> [Colours]
strike guesses results = filter (isConsistent) guesses
  where isConsistent g = consistent g results

sampleResults :: [Result]
sampleResults = [([R,B,B],(1,0)), ([R,G,G],(2,0))]
