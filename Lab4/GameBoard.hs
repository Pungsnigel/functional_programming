module GameBoard where
import Graphics.Gloss

data World = World { 
                     population :: Board,   -- The population - Matrix of Tiles
                     width :: Int,          -- Number of tiles in the base
                     tileSize :: Int,       -- Size of each tile (width and height always the same)
                     allowedTime :: Float,  -- The time between updates
                     elapsedTime :: Float   -- The time since the last update
                   }
    
data Board = Board { rows :: [[Tile]] }
      deriving (Show, Eq)

data Tile = Alive | Dead
    deriving Eq

type Pos = (Int,Int)

instance Show Tile where
    show Alive = "*"
    show Dead  = "o"

-- Generate a 10x10 blank board
allBlankBoard :: Board
allBlankBoard = Board $ replicate 50 $ replicate 50 Dead

example :: Board
example = Board [
                  [Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead],
                  [Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead],
                  [Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead],
                  [Dead, Dead, Dead, Alive, Alive, Dead, Dead, Dead, Dead, Dead],
                  [Dead, Dead, Dead, Alive, Alive, Dead, Dead, Dead, Dead, Dead],
                  [Dead, Dead, Dead, Dead, Dead, Alive, Alive, Dead, Dead, Dead],
                  [Dead, Dead, Dead, Dead, Dead, Alive, Alive, Dead, Dead, Dead],
                  [Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead],
                  [Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead],
                  [Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead]
                  ]

defaultWorld :: World
defaultWorld = World {population = example, width = length $ rows example, allowedTime = 0.2, 
            elapsedTime = 0, tileSize = 10}

-- Inserts an element at a given position of a list
(!!=) :: [a] -> (Int, a) -> [a]
(!!=) l (p, e) 
    | p >= length l = l
    | otherwise     = take p l ++ [e] ++ drop (p+1) l

-- Insert a tile at a given position in a board
update :: Board -> Pos -> Tile -> Board
update board (row,col) val = Board (b !!= (row,newRow))
    where b                = rows board
          newRow           = (b !! row) !!= (col,val)

-- Get the value of the tile att the given position
getTile :: Board -> Pos -> Tile
getTile b (r,c)  
  | r < 0 || c < 0 || r > height || c > width = Dead
  | otherwise                                 = (rows b !! r) !! c
      where height     = length (rows b) - 1
            width      = length (head $ rows b) - 1

-- Given a board and a position, return the number of alive neighbors
calculateNeigbours :: Board -> Pos -> Int
calculateNeigbours b (r,c) = length [tile | tile <- neighbors, getTile b tile == Alive]
    where neighbors = [(r+1,c), (r-1,c), (r+1,c+1), (r,c+1), (r,c-1), (r-1,c-1), (r-1,c+1), (r+1,c-1)]

{-Generate a matrix of ints, where each value corresponts to the number of alive 
neighbors at that position of the board -}
getNeighbourMatrix :: Board -> [[Int]]
getNeighbourMatrix b = [map (calculateNeigbours b) pos | pos <- indexes]
    where indexes    = [[(x,y) | y <- [0..width]] | x <- [0..height]]
          height     = length  (rows b) - 1
          width      = length (head $ rows b) - 1

-- Update a board to the next generation
calculateNextBoard :: Board -> Board
calculateNextBoard b = Board $ zipWith (curry calculateNextRow) (rows b) p 
      where calculateNextRow = uncurry (zipWith (curry calculateCell))
            p = getNeighbourMatrix b

-- Decide if the given tile should be dead or alive
calculateCell :: (Tile, Int) -> Tile
calculateCell (Dead, 3)  = Alive
calculateCell (Alive, 2) = Alive
calculateCell (Alive, 3) = Alive
calculateCell _          = Dead

