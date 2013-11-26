module Sudoku where

import Test.QuickCheck
import Data.Maybe
import Data.Char
import Data.List

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
      , [Nothing,Nothing,Just 7, Just 6, Just 9, Nothing,Nothing,Just 4, Just 3]
      ]

-- allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku $ replicate 9 $ replicate 9 Nothing

-- isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku sudoku         = (length (rows sudoku) == 9) && (and [check row | row <- rows sudoku])
        where check row = (length row == 9) && (and [(fromMaybe 1 y) `elem` [1..9] | y <- row])

-- isSolved sud checks if sud is already solved, i.e. there are no blanks
isSolved :: Sudoku -> Bool
isSolved (Sudoku sud) = Nothing `notElem` concat sud

-------------------------------------------------------------------------

-- printSudoku sud prints a representation of the sudoku sud on the screen
printSudoku :: Sudoku -> IO ()
printSudoku sudoku = do 
  putStr $ unlines [map (maybe '.' intToDigit) row | row <- rows sudoku]

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

-- an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Sudoku rows)

prop_sudoku :: Sudoku -> Bool
prop_sudoku sud = isSudoku sud
-------------------------------------------------------------------------

type Block = [Maybe Int]

isOkayBlock :: Block -> Bool
isOkayBlock block = catMaybes block == catMaybes (nub block)

blocks :: Sudoku -> [Block]
blocks sud = rows sud ++ (transpose $ rows sud) ++ getBlocks sud
    where  getBlocks sud          = concat [getBlocksOnRow (rows sud) row | row <- [0,3,6]] 
           getBlocksOnRow sud row = [getBlock sud row col | col <- [0,3,6]]
           getBlock sud row col   = concat 
                [drop col $ take (col+3) rows | rows <- drop row $ take (row+3) sud]

isOkay :: Sudoku -> Bool
isOkay sud = all isOkayBlock (blocks sud)

prop_blocks sud = length (blocks sud) == 27 && and [length block == 9 | block <- blocks sud]
-------------------------------------------------------------------------

type Pos = (Int, Int)

blanks :: Sudoku -> [Pos]
blanks sudoku = [pos | (pos,val) <- positions sudoku, val == Nothing]
    where positions sud = zip indexes (concat $ rows sud)
          indexes       = [(x,y) | x <- [0..8], y <- [0..8]]

prop_blanksPosition :: Sudoku -> Bool
prop_blanksPosition sud = all isBlank (blanks sud)
    where isBlank (row,col) = (head $ drop col (head $ drop row (rows sud))) == Nothing

(!!=) :: [a] -> (Int, a) -> [a]
(!!=) l (p, e) 
    | p >= length l = l
    | otherwise     = take p l ++ [e] ++ drop (p+1) l 

prop_insertLength :: [a] -> Bool
prop_insertLength l = length l == length (l !!= (0,last l))

prop_didInsert ::(Eq a) => [a] -> Bool
prop_didInsert l = length l == 0 || head (l !!= (0, last l)) == last l

update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update sudoku (row,col) val = Sudoku (sud !!= (row,newRow))
    where sud               = rows sudoku
          newRow            = (sud !! row) !!= (col,val)

candidates :: Sudoku -> Pos -> [Int]
candidates sud pos = [x | x <- [1..9], isOkay $ update sud pos (Just x)]

prop_candidatesAreValid :: Sudoku -> Bool
prop_candidatesAreValid sud = and [candidates_validForPos (row,col) | row <- [0..8], col <- [0..8]]
      where candidates_validForPos pos = and [(isOkay $ updatedSudoku x pos) && 
                                            (isSudoku $ updatedSudoku x pos) | x <- candidates sud pos]
            updatedSudoku x position   = update sud position (Just x)

-------------------------------------------------------------------------
solve :: Sudoku -> Maybe Sudoku
solve sudoku
    | (not $ isOkay sudoku) || (not $ isSudoku sudoku) = Nothing
    | otherwise                                        = solve' sudoku
      where solve' sud
              | null $ blanks sud = Just sud
              | otherwise         = listToMaybe $ catMaybes [solve $ update sud pos (Just cand) | cand <- candidates sud pos]
                         where pos       = head $ blanks sud

readAndSolve :: FilePath -> IO ()
readAndSolve path= do 
    sudoku <- readSudoku path
    if (solve sudoku) == Nothing
      then putStrLn "No solution"
      else printSudoku $ fromJust (solve sudoku)

isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf sud1 sud2 = (isSolved sud1) &&
            (and [b == Nothing | (a,b) <- zipped, a /= b])
        where zipped = zip (concat $ rows sud1) (concat $ rows sud2)


    

