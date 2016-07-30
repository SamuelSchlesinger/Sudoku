import Z3.Monad
import Data.List
import Control.Monad.Trans (liftIO)
import Control.Monad (forM, void, (=<<))
import System.IO.Unsafe
import Control.Arrow (first)
import Data.Maybe (isJust, fromJust, isNothing)

compose :: Eq a => [(a, b)] -> [a] -> [b]
compose xs = map fromJust . filter isJust . map (`lookup` xs) 

checkBoard :: [((Int, Int), Integer)] -> Bool
checkBoard board = range && distinct where 
  b = map snd board
  range = all (\x -> 1 <= x && x <= 9) b
  distinct = all (\x -> nub x == x) sections
  sections = do
    domain <- [quadrants, rows, cols]
    realm <- domain
    pure $ compose (map (first place) board) realm 

-- | A bijection Z2[9,9] -> Z[81]
place :: (Int, Int) -> Int
place (n, m) = n + m*9
unplace :: Int -> (Int, Int)
unplace n = (mod n 9,  div n 9)

-- | A Sudoku board mapped via the place bijection
board :: [Int]
board = [place(i, j) | i <- [0..8], j <- [0..8]]

-- | The quadrants of the Sudoku board
quadrants :: [[Int]]
quadrants = [[place(i*3 + x, j*3 + y) | x <- [0..2], y <- [0..2]] | i <- [0..2], j <- [0..2]]

-- | The rows of the Sudoku board
rows :: [[Int]]
rows = [[place(i, j) | j <- [0..8]] | i <- [0..8]]

-- | The columns of the Sudoku board
cols :: [[Int]]
cols = [[place(j, i) | j <- [0..8]] | i <- [0..8]]

-- | One test which the rows, cols, and quadrants must satisfy
eqCondition1 = all ((== sort board) . sort) $ map concat [quadrants, rows, cols]

mkVars :: Z3 [(Int, AST)]
mkVars = mapM (\x -> do { xsym <- mkIntSymbol x; xvar <- mkIntVar xsym; pure (x, xvar) }) board

mkRanges :: [AST] -> Z3 [AST]
mkRanges xs = do
  one <- mkInteger 1
  nine <- mkInteger 9
  sequence ([mkLe one, mkGe nine] <*> xs)

mkAssignments :: [(Int, AST)] -> [((Int, Int), Int)] -> Z3 ()
mkAssignments vs bs = void $ forM (map (first place) bs)
  $ \(n, a) -> case lookup n vs of
    Nothing -> pure ()
    Just x  -> do
      i <- mkIntNum a
      e <- mkEq x i
      assert e

mainLogic :: [((Int, Int), Int)] -> Z3 ()
mainLogic as = do
  vars <- mkVars
  let vs = map snd vars
  rs <- mkRanges vs
  mapM_ assert rs
  mkAssignments vars as
  let qasts = map (compose vars) quadrants
  let rasts = map (compose vars) rows
  let casts = map (compose vars) cols
  mapM_ assert =<< mapM mkDistinct qasts
  mapM_ assert =<< mapM mkDistinct rasts
  mapM_ assert =<< mapM mkDistinct casts
  c <- check
  liftIO $ print c
  model <- solverGetModel
  maybeResults <- mapM (\x -> modelEval model x True) vs
  if any isNothing maybeResults 
    then liftIO $ print "Solver failed to find a solution to the Sudoku instance"
    else do
      let ms = map fromJust maybeResults
      maybeis <- mapM (evalInt model) ms
      let is = map fromJust maybeis
      liftIO $ print "Checking board..."
      liftIO $ print $ checkBoard $ zip (map unplace board) is
      liftIO $ print "Printing board:"
      liftIO $ print $ zip (map unplace board) is

main = do
  evalZ3 $ mainLogic []
