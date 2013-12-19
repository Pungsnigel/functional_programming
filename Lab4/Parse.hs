module Parse where
import GameBoard
import Parsing

stringToNewGame :: String -> Board
stringToNewGame string = map stringToRow $ lines string

stringToRow :: String -> [Tile]
stringToRow string = map charToTile string
    where charToTile '.' = Dead
          charToTile '*' = Alive
          charToTile  _  = error "Parse error: Invalid character in file!"
