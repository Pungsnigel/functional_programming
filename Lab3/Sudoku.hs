module Sudoku where

import Test.QuickCheck
import Data.Maybe
import Data.Char

-------------------------------------------------------------------------

data Sudoku = Sudoku { rows :: [[Maybe Int]] }
 deriving ( Show, Eq )

example :: Sudoku
example =
    Sudoku
      [ [Just 3, Just 6, Nothing,Nothing,Just 7, Just 1, Just 2, Nothing,Nothing]
      , [Nothing,Just 5, Nothing,Nothing,Nothing,Nothing,Just 1, Just 8, Nothing]
      , [Nothing,Nothing,Just 9, Just 2, Nothing,Just 4, Just 7, Nothing,Nothing]
      , [Nothing,Nothing,Nothing,Nothing,Just 1, Just 3, Nothing,Just 2, Just 8]
      , [Just 4, Nothing,Nothing,Just 5, Nothing,Just 2, Nothing,Nothing,Just 9]
      , [Just 2, Just 7, Nothing,Just 4, Just 6, Nothing,Nothing,Nothing,Nothing]
      , [Nothing,Nothing,Just 5, Just 3, Nothing,Just 8, Just 9, Nothing,Nothing]
      , [Nothing,Just 8, Just 3, Nothing,Nothing,Nothing,Nothing,Just 6, Nothing]
      , [Nothing,Nothing,Just 7, Just 6, Just 1, Nothing,Nothing,Just 4, Just 3]
      ]

-- allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku (replicate 9 (replicate 9 Nothing))

-- isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku sudoku 
    | length (rows sudoku) /= 9             = False
    | otherwise                             = and [check row | row <- rows sudoku]
        where check row | length row /= 9   = False 
                        | otherwise         = and [(fromMaybe 1 y) `elem` [1..9] | y <- row]

-- isSolved sud checks if sud is already solved, i.e. there are no blanks
isSolved :: Sudoku -> Bool
isSolved sudoku = and [check row | row <- rows sudoku]
    where check row = and [y /= Nothing | y <- row]

-------------------------------------------------------------------------

-- printSudoku sud prints a representation of the sudoku sud on the screen
printSudoku :: Sudoku -> IO ()
printSudoku sudoku = do 
  putStr $ unlines [map toChar row | row <- rows sudoku]
    where toChar = maybe '.' intToDigit

-- readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku path = do 
      content <- readFile path
      return $ stringToSudoku content


stringToSudoku :: String -> Sudoku
stringToSudoku string = Sudoku [map toSud row | row <- lines string]
  where toSud '.' = Nothing
        toSud c   = Just (digitToInt c)

-------------------------------------------------------------------------

-- cell generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell = frequency [
          (9,   return Nothing),
          (1,   rNum)
          ]

rNum :: Gen (Maybe Int)
rNum = do 
    n <- choose (1,9)
    return $ Just n

-- natural numbers
nats :: Gen Integer
nats = do
     i <- arbitrary
     return $ abs i

-- an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Sudoku rows)

prop_sudoku :: Sudoku -> Bool
prop_sudoku sud = isSudoku sud
-------------------------------------------------------------------------
