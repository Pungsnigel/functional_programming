data Board = Board { rows :: [[Tile]] }
    deriving (Show, Eq)

data Tile = Alive | Dead 
    deriving Eq

type Pos = (Int,Int)

instance Show Tile where
    show Alive = "*"
    show Dead  = "o"

allBlankBoard :: Board
allBlankBoard = Board $ replicate 10 $ replicate 10 Dead

-- Inserts an element at a given position of a list
(!!=) :: [a] -> (Int, a) -> [a]
(!!=) l (p, e) 
    | p >= length l = l
    | otherwise     = take p l ++ [e] ++ drop (p+1) l

update :: Board -> Pos -> Tile -> Board
update board (row,col) val = Board (b !!= (row,newRow))
    where b                = rows board
          newRow           = (b !! row) !!= (col,val)
{-
nrOfLivingNeighbours :: Board -> ((Int))
nrOfLivingNeighbours 
    where positions b = zip indexes (concat $ rows b)
          indexes = [(x,y) | x <- [0..width], y <- [0..height]]
          height    = (length $ rows b) - 1
          width     = (length $ head $ rows b) - 1

-}
calculateNextBoard :: Board -> [[Integer]] -> Board
calculateNextBoard b p = Board $ map calculateNextRow $ zip (rows b) p 
      where calculateNextRow tup = map calculateCell $ zip (fst tup) (snd tup)

calculateCell :: (Tile, Integer) -> Tile
calculateCell (Dead, 3)  = Alive
calculateCell (Alive, 2) = Alive
calculateCell (Alive, 3) = Alive
calculateCell _          = Dead
