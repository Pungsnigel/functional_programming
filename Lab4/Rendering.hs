module Rendering where
import GameBoard
import Graphics.Gloss

renderWorld :: World -> Picture
renderWorld world = Pictures [renderTile tile xOff yOff (fromIntegral size) | (tile, xOff, yOff, size) <- tileList]
	where tileList = [(t,x,y, tileSize world) | (t,(x,y)) <- getTileOffsets world ]

getTileOffsets :: World -> [(Tile, (Int, Int))]
getTileOffsets (World b wSize tSize _ _) = zip (concat $ rows b) (concat offsets)
		where offsets    = [[(tSize * x ,tSize * y) | y <- (reverse [0..wSize-1])] | x <- [0..wSize - 1]]

renderTile :: Tile -> Int -> Int -> Float -> Picture
renderTile tile posX posY size
	| tile == Dead = Blank
	| otherwise    = Translate (fromIntegral posX) (fromIntegral posY) $ rectangleSolid size size

getDrawSize :: World -> Int
getDrawSize world = (tileSize world) * (width world)
