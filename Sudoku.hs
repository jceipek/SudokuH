import Data.List (intersect)
import Data.List (sortBy)

puzzle :: [[[Int]]]

-- Simple enough to solve without assumptions
{-
puzzle = [[[ ],[ ],[7],[ ],[6],[9],[5],[ ],[1]],
            [[8],[ ],[5],[ ],[7],[ ],[9],[ ],[6]],
            [[6],[9],[ ],[5],[ ],[8],[2],[ ],[ ]],
            [[ ],[6],[1],[ ],[4],[3],[ ],[2],[ ]],
            [[5],[ ],[ ],[1],[9],[ ],[ ],[7],[3]],
            [[ ],[7],[4],[ ],[ ],[5],[1],[6],[ ]],
            [[ ],[8],[ ],[9],[ ],[ ],[ ],[5],[2]],
            [[4],[ ],[9],[2],[5],[ ],[ ],[1],[8]],
            [[7],[5],[ ],[6],[ ],[1],[3],[ ],[ ]]]
-}

-- *Main> puzzleDisplay (dfsSolve (buildPuzzleState puzzle (3,3)))
-- [[2],[4],[7],[3],[6],[9],[5],[8],[1]]
-- [[8],[1],[5],[4],[7],[2],[9],[3],[6]]
-- [[6],[9],[3],[5],[1],[8],[2],[4],[7]]
-- [[9],[6],[1],[7],[4],[3],[8],[2],[5]]
-- [[5],[2],[8],[1],[9],[6],[4],[7],[3]]
-- [[3],[7],[4],[8],[2],[5],[1],[6],[9]]
-- [[1],[8],[6],[9],[3],[4],[7],[5],[2]]
-- [[4],[3],[9],[2],[5],[7],[6],[1],[8]]
-- [[7],[5],[2],[6],[8],[1],[3],[9],[4]]


-- Easy puzzle
puzzle = [[[ ],[ ],[ ],[4],[ ],[5],[ ],[ ],[ ]],
          [[ ],[9],[ ],[8],[ ],[1],[ ],[7],[ ]],
          [[2],[ ],[ ],[9],[6],[3],[ ],[ ],[8]],
          [[ ],[ ],[7],[ ],[ ],[ ],[3],[ ],[ ]],
          [[5],[ ],[8],[ ],[ ],[ ],[4],[ ],[1]],
          [[ ],[ ],[6],[ ],[ ],[ ],[8],[ ],[ ]],
          [[7],[ ],[ ],[5],[1],[6],[ ],[ ],[3]],
          [[ ],[4],[ ],[7],[ ],[2],[ ],[5],[ ]],
          [[ ],[ ],[ ],[3],[ ],[9],[ ],[ ],[ ]]]

-- *Main> puzzleDisplay (dfsSolve (buildPuzzleState puzzle (3,3)))
-- [[8],[6],[1],[4],[7],[5],[9],[3],[2]]
-- [[3],[9],[5],[8],[2],[1],[6],[7],[4]]
-- [[2],[7],[4],[9],[6],[3],[5],[1],[8]]
-- [[4],[1],[7],[2],[9],[8],[3],[6],[5]]
-- [[5],[2],[8],[6],[3],[7],[4],[9],[1]]
-- [[9],[3],[6],[1],[5],[4],[8],[2],[7]]
-- [[7],[8],[9],[5],[1],[6],[2],[4],[3]]
-- [[6],[4],[3],[7],[8],[2],[1],[5],[9]]
-- [[1],[5],[2],[3],[4],[9],[7],[8],[6]]

-- Hard puzzle; not solved after 9 hours
{-
puzzle = [[[ ],[ ],[ ],[8],[4],[ ],[ ],[ ],[9]],
          [[ ],[ ],[1],[ ],[ ],[ ],[ ],[ ],[5]],
          [[8],[ ],[ ],[ ],[2],[1],[4],[6],[ ]],
          [[7],[ ],[8],[ ],[ ],[ ],[ ],[9],[ ]],
          [[ ],[ ],[ ],[ ],[ ],[ ],[ ],[ ],[ ]],
          [[ ],[5],[ ],[ ],[ ],[ ],[3],[ ],[1]],
          [[ ],[2],[4],[9],[1],[ ],[ ],[ ],[7]],
          [[9],[ ],[ ],[ ],[ ],[ ],[5],[ ],[ ]],
          [[3],[ ],[ ],[ ],[8],[4],[ ],[ ],[ ]]]
-}

------------------------------------
-- GENERAL HELPER FUNCTIONS --
------------------------------------

-- Generate a list of every possible (x,y) position on a grid with given dimensions
generateLocList :: (Int,Int) -> [(Int,Int)]
generateLocList dims = [(x,y) | x <- [0..(xDim - 1)], y <- [0..(yDim - 1)]]
    where (xDim,yDim) = dims

-- Given a list, list all possible numbers that could appear in it, starting with 1
-- This is currently just the numbers from 1 to the length of the list
allPossibleOptions :: [[Int]] -> [Int]
allPossibleOptions lst = [1..(length lst)]

-- Given a list, of [Int] and [] elements, determine all of the numbers missing from it
missingFromList :: [[Int]] -> [Int]
missingFromList lst = [x | x <- allPossibleOptions lst, [x] `notElem` lst]

-- Given a puzzle and an Int index (0-indexed), return the row at that index as a list
getRow :: [[[Int]]] -> Int -> [[Int]]
getRow p idx = p !! idx

-- Given a puzzle and an Int index (0-indexed), return the column at that index as a list
getColumn :: [[[Int]]] -> Int -> [[Int]]
getColumn p idx = [x !! idx | x <- p]

-- Obtain the dimensions of the puzzle
getDims :: [[[Int]]] -> (Int, Int)
getDims p = ((length (getColumn p 0)), (length (getRow p 0)))

-- Given a location and a region size, returns the corresponding region coordinates
getRegionForLoc :: (Int,Int) -> (Int,Int) -> (Int,Int)
getRegionForLoc dims loc = (x,y)
    where (rxDim,ryDim) = dims
          (locX,locY) = loc
          x = floor (fromIntegral locX / fromIntegral rxDim)
          y = floor (fromIntegral locY / fromIntegral ryDim)

-- Obtain a region of size dims (generally (3,3)) of the puzzle at region loc
-- and then up and down. On a standard Sudoku puzzle, the the middle bottom region has index 7
getRegionHelper :: [[[Int]]] -> (Int, Int) -> (Int,Int) -> [[[Int]]]
getRegionHelper p dims loc = [take rxDim (drop (xLoc * rxDim) x) | x <- (take ryDim (drop (yLoc * ryDim) p))]
    where (pxDim,pyDim) = getDims p
          (rxDim,ryDim) = dims
          (xLoc,yLoc) = loc

-- Get a region of the puzzle using getRegionHelper. Make it a list.
getRegion :: [[[Int]]] -> (Int, Int) -> (Int,Int) -> [[Int]]
getRegion p dims loc = concat (getRegionHelper p dims loc)

-- Get all rows in the puzzle
getRows :: [[[Int]]] -> [[[Int]]]
getRows p = p

-- Get all columns in the puzzle
getColumns :: [[[Int]]] -> [[[Int]]]
getColumns p = [getColumn p (x-1) | x <- [1..colCount]]
    where (colCount,_) = getDims p

-- Get all regions of size dims (generally (3,3)) in the puzzle
getRegions :: [[[Int]]] -> (Int,Int) -> [[[Int]]]
getRegions p dims = [getRegion p dims x | x <- generateLocList dims]

-- Checks if all elements in the list are unique or empty lists
isValidList :: [[Int]] -> Bool
isValidList [] = True
isValidList (x:xs) = ((x == []) || (x `notElem` xs)) && (isValidList xs)

-- Checks if a puzzle with given region dimensions is valid by Sudoku rules
isValidState :: [[[Int]]] -> (Int,Int) -> Bool
isValidState p regionDims = (null (filter (\x -> (isValidList x) == False) (getRows p))) &&
                 (null (filter (\x -> (isValidList x) == False) (getColumns p))) &&
                 (null (filter (\x -> (isValidList x) == False) (getRegions p regionDims)))

-- Checks if the puzzle has no blank places
isComplete :: [[[Int]]] -> Bool
isComplete p = [] `notElem` (concat p)

-- Checks if the puzzle with given region dimensions is fully and correctly solved
isSolved :: [[[Int]]] -> (Int,Int) -> Bool
isSolved p regionDims = (isComplete p) && (isValidState p regionDims)

-- Creates a new puzzle with a symbol placed at loc
placeSymbolAt :: [[[Int]]] -> [Int] -> (Int, Int) -> [[[Int]]]
placeSymbolAt p symb loc = firstRows ++ ((rowStart ++ (symb : rs)) : rest)
    where (xPos,yPos) = loc
          (firstRows,secondRows) = (splitAt yPos p)
          (row:rest) = secondRows
          (rowStart,_:rs) = (splitAt xPos row)

-- gets the symbol at a given location on the puzzle
getSymbolAt :: [[[Int]]] -> (Int, Int) -> [Int]
getSymbolAt p loc = symb
    where (xPos,yPos) = loc
          (firstRows,secondRows) = (splitAt yPos p)
          (row:rest) = secondRows
          (rowStart,symb:rs) = (splitAt xPos row)

-- Given a pozition on the puzzle and the region dimensions, determine which symbols can be placed there
missingAt :: [[[Int]]] -> (Int,Int) -> (Int,Int) -> [Int]
missingAt p regionDims loc
    | (getSymbolAt p loc) == [] = missingFromRow `intersect` missingFromColumn `intersect` missingFromRegion
    | otherwise                 = []
    where (xPos,yPos) = loc
          missingFromRow = missingFromList (getRow p yPos)
          missingFromColumn = missingFromList (getColumn p xPos)
          missingFromRegion = missingFromList (getRegion p regionDims (getRegionForLoc regionDims loc))

-- Generate a list of every possible (x,y) position on the puzzle (even occupied squares)
generateAllLocs :: [[[Int]]] -> [(Int, Int)]
generateAllLocs p = generateLocList (getDims p)

-- Display the puzzle such that each row is on its own line
puzzleDisplay :: [[[Int]]] -> IO()
puzzleDisplay p = sequence_ [putStrLn (show x) | x <- p]

------------------------------------
-- SOLVER WITH NO ASSUMPTIONS --
------------------------------------

-- Place the appropriate symbol at loc if it is guaranteed to be correct; return the puzzle and whether a change was made
applyForcedPlacementAt :: [[[Int]]] -> (Int,Int) -> (Int,Int) -> ([[[Int]]], Bool)
applyForcedPlacementAt p regionDims loc
    | (length miss == 1) = (placeSymbolAt p [i] loc, True)
    | otherwise                   = (p, False)
    where miss  = (missingAt p regionDims loc)
          (i:_) = miss

-- Recursively try applyForcedPlacementAt on all locations passed in until it succeeds
-- Returns the new state of the puzzle and whether it has been changed
solveForcedHelper :: [[[Int]]] -> (Int, Int) -> [(Int,Int)] -> ([[[Int]]], Bool)
solveForcedHelper p regionDims [] = (p, False)
solveForcedHelper p regionDims (currLocToCheck:locsToCheck)
    | didChange = (newP, True)
    | otherwise = solveForcedHelper p regionDims locsToCheck
    where (newP,didChange) = applyForcedPlacementAt p regionDims currLocToCheck

-- Recursively try solveForcedHelper until it can make no more progress. Then, return the new puzzle state
solveForcedRecurser :: ([[[Int]]], Bool) -> (Int, Int) -> [(Int,Int)] -> [[[Int]]]
solveForcedRecurser (p, False) regionDims locsToCheck = p
solveForcedRecurser (p, True) regionDims locsToCheck = solveForcedRecurser (solveForcedHelper p regionDims locsToCheck) regionDims locsToCheck

-- Try to come up with a solution without ever assuming anything
solveForced :: [[[Int]]] -> (Int,Int) -> [[[Int]]]
solveForced p regionDims = solveForcedRecurser (p, True) regionDims locsToCheck
    where (pxDim,pyDim) = getDims p
          locsToCheck = generateAllLocs p

------------------------------------
-- AI SOLVER --
------------------------------------
buildPuzzleState :: [[[Int]]] -> (Int,Int) -> ([[[Int]]], (Int,Int))
buildPuzzleState p regionDims = (p,regionDims)

puzzleStatePuzzle :: ([[[Int]]], (Int,Int)) -> [[[Int]]]
puzzleStatePuzzle ps = fst ps

puzzleStateRegionDims :: ([[[Int]]],(Int,Int)) -> (Int,Int)
puzzleStateRegionDims ps = snd ps

buildMove :: (Int,Int) -> [[Int]] -> ((Int,Int),[[Int]])
buildMove loc options = (loc, options)

moveLoc :: ((Int,Int),[[Int]]) -> (Int,Int)
moveLoc ms = fst ms

moveOptions :: ((Int,Int),[[Int]]) -> [[Int]]
moveOptions ms = snd ms

generatePossibleMoves :: ([[[Int]]], (Int,Int)) -> [((Int,Int),[[Int]])]
generatePossibleMoves ps = [buildMove loc [[x] | x <- (missingAt p (puzzleStateRegionDims ps) loc)] | loc <- (generateAllLocs p)]
    where p = (puzzleStatePuzzle ps)

eliminateInvalidMoves :: [((Int,Int),[[Int]])] -> [((Int,Int),[[Int]])]
eliminateInvalidMoves moves = filter (\m -> (moveOptions m) /= []) moves

sortMoves :: [((Int,Int),[[Int]])] -> [((Int,Int),[[Int]])]
sortMoves moves = sortBy (\ x y -> compare (length (moveOptions x)) (length (moveOptions y))) moves

generateGoodMovesFirst :: ([[[Int]]], (Int,Int)) -> [((Int,Int),[[Int]])]
generateGoodMovesFirst ps = sortMoves (eliminateInvalidMoves (generatePossibleMoves ps))

dfsSolveHelperHelper :: ([[[Int]]], (Int,Int)) -> ((Int,Int),[[Int]]) -> ([[[Int]]], (Int,Int))
dfsSolveHelperHelper ps (loc,[]) = ps
dfsSolveHelperHelper ps (loc,(s:ss))
    | isComplete newP = newState
    | null possMoves = dfsSolveHelperHelper ps (loc,ss)
    | otherwise = dfsSolveHelper newState possMoves
    where p = (puzzleStatePuzzle ps)
          regionDims = (puzzleStateRegionDims ps)
          newP = (placeSymbolAt p s loc)
          newState = buildPuzzleState newP regionDims
          possMoves = (generateGoodMovesFirst newState)

dfsSolveHelper :: ([[[Int]]], (Int,Int)) -> [((Int,Int),[[Int]])] -> ([[[Int]]], (Int,Int))
dfsSolveHelper ps [] = ps
dfsSolveHelper ps (m:moves)
    | isComplete (puzzleStatePuzzle newState) = newState
    | otherwise = dfsSolveHelper ps moves
    where newState = dfsSolveHelperHelper ps m

dfsSolve :: ([[[Int]]], (Int,Int)) -> [[[Int]]]
dfsSolve ps = puzzleStatePuzzle (dfsSolveHelper ps (generateGoodMovesFirst ps))

main = print (solveForced puzzle (3,3))
