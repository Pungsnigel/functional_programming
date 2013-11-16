data Sudoku = Sudoku {rows :: [[Maybe Int]] }
    deriving Show

allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku (makeBlankSudoku 9 [])
    where makeBlankSudoku 0 m = m
          makeBlankSudoku n m = makeBlankSudoku (n-1) ((makeBlankRow 9 []):m)
                where makeBlankRow 0 xs = xs
                      makeBlankRow n xs = makeBlankRow (n-1) (Nothing:xs)
