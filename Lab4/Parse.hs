module Parse where
import GameBoard

stringToNewGame :: String -> Board
stringToNewGame string = Board $ map stringToRow $ lines string

stringToRow :: String -> [Tile]
stringToRow string = map charToTile string
    where charToTile '.' = Dead
          charToTile '*' = Alive
          charToTile  _  = error "Parse error: Invalid character in file!"

parseWorld :: FilePath -> IO World
parseWorld path = do
	content <- readFile path
	board <- return $ stringToNewGame content
	return $ World board (length $ rows board) 5 0.1 0
