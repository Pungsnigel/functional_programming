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

example :: Board
example = Board [[Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead],
                  [Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead],
                  [Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead],
                  [Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead],
                  [Dead, Dead, Dead, Dead, Alive, Dead, Dead, Dead, Dead, Dead],
                  [Dead, Dead, Dead, Dead, Alive, Dead, Dead, Dead, Dead, Dead],
                  [Dead, Dead, Dead, Dead, Alive, Dead, Dead, Dead, Alive, Dead],
                  [Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Alive, Dead],
                  [Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Alive, Dead],
                  [Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead]
                  ]

-- Inserts an element at a given position of a list
(!!=) :: [a] -> (Int, a) -> [a]
(!!=) l (p, e) 
    | p >= length l = l
    | otherwise     = take p l ++ [e] ++ drop (p+1) l

update :: Board -> Pos -> Tile -> Board
update board (row,col) val = Board (b !!= (row,newRow))
    where b                = rows board
          newRow           = (b !! row) !!= (col,val)

getTile :: Board -> Pos -> Tile
getTile b (r,c)  
  | r < 0 || c < 0 || r > height || c > width = Dead
  | otherwise                                 = (rows b !! r) !! c
      where height     = length (rows b) - 1
            width      = length (head $ rows b) - 1

calculateNeigbours :: Board -> Pos -> Int
calculateNeigbours b (r,c) = length [tile | tile <- neighbors, getTile b tile == Alive]
    where neighbors = [(r+1,c), (r-1,c), (r+1,c+1), (r,c+1), (r,c-1), (r-1,c-1), (r-1,c+1), (r+1,c-1)]

getNeighbourMatrix :: Board -> [[Int]]
getNeighbourMatrix b = [map (calculateNeigbours b) row | row <- indexes]
    where indexes    = [[(x,y) | y <- [0..width]] | x <- [0..height]]
          height     = length  (rows b) - 1
          width      = length (head $ rows b) - 1

calculateNextBoard :: Board -> Board
calculateNextBoard b = Board $ zipWith (curry calculateNextRow) (rows b) p 
      where calculateNextRow = uncurry (zipWith (curry calculateCell))
            p = getNeighbourMatrix b

calculateCell :: (Tile, Int) -> Tile
calculateCell (Dead, 3)  = Alive
calculateCell (Alive, 2) = Alive
calculateCell (Alive, 3) = Alive
calculateCell _          = Dead
