module Main where
import GameBoard
import Rendering
import System.Environment
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Simulate
import Data.Maybe
import Data.Char
import Parse

main :: IO ()
main = do	
	args  <- getArgs
	world <- if null args
		then
			return defaultWorld
		else
			parseWorld $ head args
	let windowSize = getDrawSize world
	simulate (InWindow "Game of Life" 
           	           (windowSize, windowSize) (5, 5))
		white 10 world renderWorld evolveWorld 

parseWorld :: FilePath -> IO World
parseWorld path = do
	content <- readFile path
	board <- return $ stringToNewGame content
	return $ World board (length $ rows board) 5 0.2 0

-- Update the population of a world to the next generation
stepWorld :: World -> World
stepWorld world
  = world { population = calculateNextBoard $ population world }

-- Function for evolving a world, one step at a time
evolveWorld :: ViewPort -> Float -> World -> World
evolveWorld _ ds world 
  | elapsedTime world >= (allowedTime world)
    = let newWorld  = stepWorld world 
        in  newWorld { elapsedTime = 0 }
  | otherwise = world { elapsedTime = elapsedTime world + ds }
	