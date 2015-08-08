-- Author: Markus Kusano
--
-- Description of 2D platformer level.
--
-- The level is described by a grid where each grid can have 0--4 "doors",
-- one on each cardinal direction.
module Level (GridDesc
             , blocks
             , gridWidth
             , gridHeight
             , OpenDir
             , Block
             , northOpen
             , southOpen
             , eastOpen
             , westOpen
             , openDirToBlock
             , blockToOpenDir
             , createClosedGrid
             , toASCII
             , getPointsWithin
             , testGrid1
             , testGrid2
             , dfsMaze) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (isJust
                  , fromJust)
import Control.Monad.Random (MonadRandom
                            , getRandomR)
import Control.Monad (guard)

-- A block can be open in any one of the cardinal directions. If the block
-- is open in a direction, then the corresponding record will be true.
data Block = Block { 
                    northOpen :: Bool
                    , southOpen :: Bool
                    , eastOpen :: Bool
                    , westOpen :: Bool
                   }
                   deriving (Show, Read)

-- Something can be open in any of the cardinal directions or not open at
-- all (Z)
data OpenDir = Open'N | Open'S | Open'E | Open'W | Open'Z
             | Open'NS | Open'NE | Open'NW 
             | Open'SE | Open'SW 
             | Open'EW
             | Open'NSE | Open'NSW | Open'NEW
             | Open'SEW 
             | Open'NSEW
             deriving (Show, Read)

-- Description of a collection of blocks arranged in a grid. The grid is of
-- size `width` * `height`. For each item in the grid, the map `blocks`
-- contains the corresponding block. Keys in the `blocks` map are `(width,
-- height)` (like a standard cartesian grid).
--
-- Indices in the map grid are zero indexed. For example, if the width of
-- the grid is set to 40 then the indicies will be 0...39.
data GridDesc = GridDesc { 
                          blocks :: M.Map (Int, Int) OpenDir
                          , gridWidth :: Int
                          , gridHeight :: Int
                         }
                         deriving (Show, Read)

-- The cardinal directions
data CardDir = North | South | East | West
             deriving (Show, Read)

-- Given a block, return the direction it is open on
blockToOpenDir :: Block -> OpenDir
blockToOpenDir (Block True True True True)     = Open'NSEW
blockToOpenDir (Block True True True False)    = Open'NSE
blockToOpenDir (Block True True False True)    = Open'NSW
blockToOpenDir (Block True True False False)   = Open'NS
blockToOpenDir (Block True False True True)    = Open'NEW
blockToOpenDir (Block True False True False)   = Open'NE
blockToOpenDir (Block True False False True)   = Open'NW
blockToOpenDir (Block True False False False)  = Open'N
blockToOpenDir (Block False True True True)    = Open'SEW
blockToOpenDir (Block False True True False)   = Open'SE
blockToOpenDir (Block False True False True)   = Open'SW
blockToOpenDir (Block False True False False)  = Open'S
blockToOpenDir (Block False False True True)   = Open'EW
blockToOpenDir (Block False False True False)  = Open'E
blockToOpenDir (Block False False False True)  = Open'W
blockToOpenDir (Block False False False False) = Open'Z

-- Given an OpenDir, return it in Block form
openDirToBlock :: OpenDir -> Block
openDirToBlock Open'NSEW = (Block True True True True)
openDirToBlock Open'NSE  = (Block True True True False)
openDirToBlock Open'NSW  = (Block True True False True)
openDirToBlock Open'NS   = (Block True True False False)
openDirToBlock Open'NEW  = (Block True False True True)
openDirToBlock Open'NE   = (Block True False True False)
openDirToBlock Open'NW   = (Block True False False True)
openDirToBlock Open'N    = (Block True False False False)
openDirToBlock Open'SEW  = (Block False True True True)
openDirToBlock Open'SE   = (Block False True True False)
openDirToBlock Open'SW   = (Block False True False True)
openDirToBlock Open'S    = (Block False True False False)
openDirToBlock Open'EW   = (Block False False True True)
openDirToBlock Open'E    = (Block False False True False)
openDirToBlock Open'W    = (Block False False False True)
openDirToBlock Open'Z    = (Block False False False False)

-- A block which is closed on all sides (i.e., it cannot be accessed)
closedBlock :: Block
closedBlock = Block False False False False

-- Create a map of size `width` * `height` with each block being closed on
-- all sides.
createClosedGrid :: Int -> Int -> GridDesc
createClosedGrid w h = GridDesc { blocks = m, gridWidth = w, gridHeight = h }
  where ps = getAllCoordPairs w h
        -- Pair each of the coordinate pairs with a closed block
        m = M.fromList $ zip ps $ repeat Open'Z

-- Given width and height values, generate all possible coordinate pairs.
-- E.g., given 2 2, this will generate:
-- (0, 0), (0, 1), (1, 0), (1, 1)
getAllCoordPairs :: Int -> Int -> [(Int, Int)]
getAllCoordPairs w h= do
    x <- [0..(w-1)]
    y <- [0..(h-1)]
    return (x,y)

-- Given a Grid and a point in the grid, return all the possible neighbors
-- of the point. This will only include neighbors in the cardinal
-- directions (i.e., no diagonals). This will only return neighbors which
-- are inbounds.
--
-- This will return an empty list if all the neighbors are out-of-bounds of
-- the grid. This can only happen if the passed point is out-of-bounds.
gridNeighbors :: GridDesc -> (Int, Int) -> [(Int, Int)]
gridNeighbors g (x, y)= do
    -- The x and y values can shift one position in either direction or
    -- stay the same
    neighX <- [x+1, x-1, x]
    neighY <- [y+1, y-1, y]
    guard $ pointInBounds g (neighX, neighY)
    -- Dont return a point as a neighbor of itself
    guard $ (x, y) /= (neighX, neighY)
    -- At this point, the neightbors values also includes diagonal
    -- neighbors (e.g., (x+1, y+1)). Filter only those points which are
    -- neighbors in the cardinal directions
    guard $ isJust $ getNeighborDirection (x, y) (neighX, neighY)
    return (neighX, neighY)

-- If it is inbounds, return the neighbor of the passed point in the passed
-- direction.
getNeighInDir :: GridDesc -> CardDir -> (Int, Int) -> Maybe (Int, Int)
getNeighInDir grid dir pt = if pointInBounds grid neigh
                              then Just neigh
                              else Nothing
  where neigh = getPtNeighInDir dir pt

-- Return the point adjacent to the passed point in the passed direction
getPtNeighInDir :: CardDir -> (Int, Int) -> (Int, Int)
getPtNeighInDir North (x, y) = (x, y+1)
getPtNeighInDir South (x, y) = (x, y-1)
getPtNeighInDir East  (x, y) = (x+1, y)
getPtNeighInDir West  (x, y) = (x-1, y)


-- If the two passed points, `p1` and `p2` are immediatly adjacent, return
-- the direction in which `p2` lies relative to `p1`.
--
-- For example, if `p1 = (1,1)` and `p2 = (1,2)` then this will return Just
-- North (`p2` lies right about `p1`). This only returns Just if the points
-- are touching each other. Points which are touching diagonally will
-- return Nothing
getNeighborDirection :: (Int, Int) -> (Int, Int) -> Maybe CardDir
getNeighborDirection (x1, y1) (x2, y2) 
  -- Moving y2 down one spot makes the points equal
  | x1 == x2 && y1 == (y2 - 1) = Just North
  -- Moving y2 up one spot makes the points equal
  | x1 == x2 && y1 == (y2 + 1) = Just South
  -- Moving x2 right one spot makes the points equal
  | x1 == (x2 + 1) && y1 == y2 = Just West
  -- Moving x2 left one spot makes the points equal
  | x1 == (x2 - 1) && y1 == y2 = Just East
  | otherwise = Nothing


-- Return true if the passed point is in-bounds of the pass grid.
-- Otherwise, false.
pointInBounds :: GridDesc -> (Int, Int) -> Bool
pointInBounds g (x, y) = x >= 0 
                         && y >= 0
                         && x < width
                         && y < height
    where width = gridWidth g
          height = gridHeight g

-- Generate a grid of the passed width and height using a DFS based maze
-- generator.
--
-- Since the search is randomized, this is all done in MonadRandom
dfsMaze :: (Functor m, MonadRandom m ) => Int -> Int -> m GridDesc
dfsMaze w h = do 
    -- Select a random starting point
    stX <- getRandomR (0, (w - 1))
    stY <- getRandomR (0, (h - 1))
    let startPoint = (stX, stY)
    -- Initialize the visited set with the starting point
    --let vis = S.insert startPoint S.empty 
    dfsMaze' (createClosedGrid w h) [startPoint] S.empty

-- Recursive helper function for DFSMaze. Performs a randomized DFS through
-- the passed grid. The Grid is continually updated with fewer and fewer
-- walls based on the search. The list is the search ``stack'' of locations
-- to visit. The set is the set of visitied locations
--
-- Note: the search stack should be populated with some initial position.
dfsMaze' :: (Functor m, MonadRandom m) => GridDesc 
                             -> [(Int, Int)] 
                             -> S.Set (Int, Int) 
                             -> m GridDesc
dfsMaze' grid [] _ = return grid
dfsMaze' grid st vis = 
    case unVisNeighs of
      -- In this case where there are no more unvisited neighbors, we just
      -- continue down the stack to the next item
      [] -> dfsMaze' grid (tail st) vis
      _ -> do
        -- Inside MonadRandom, select a single neighbor. Use a uniform bias.
        n <- biasedNeighSelection 0.5 cur unVisNeighs 
        -- Mark the selected node as visited
        let nextVis = S.insert n vis
        -- Update the search stack to contain the selected neighbor as well as
        -- the current node. The order here matters. We will visit the neighbor
        -- first (and continue on in a DFS) and then come back to the current
        -- node. If the current node still has unvisited neighbors, then the
        -- process repeats. 
        let nextSt = n : cur : (tail st)
        -- Update the grid so there is a path between the current point and the
        -- selected neighbor. getNeighborDirection returns Nothing if the point
        -- is not a neighbor but we have just determined it is a neighbor so
        -- this is fine.
        let dir = fromJust $ getNeighborDirection cur n 
        let nextGrid = openPointOnGrid grid cur dir
        dfsMaze' nextGrid nextSt nextVis
  where cur = head st
        -- All the unvisited neighbors of the current node
        unVisNeighs = filter (flip S.notMember vis) $ gridNeighbors grid cur

-- Perform a biased selection of the neighbors of the passed point. 
--
-- The passed Double is the probability of selecting a horizontal neighbor. If
-- it is greater than one or less than zero then this function will crash.
--
-- For example, if the probability is 0.5 then the chance of selecting
-- a horizontal or vertical neighbor will be equal. 0.75 will bias towards
-- selecting a horizontal edge (75% of the time it will be horizontal, 25%
-- vertical). Similary, values less than 0.5 will bias towards selecting
-- a vertical neighbor.
--
-- The passed point is the point who's neighbors we are examining. The passed
-- list is the list of neighbors of the point. If the list is empty, this will
-- crash.
biasedNeighSelection :: (Functor m, MonadRandom m) => Double 
                                        -> (Int, Int) 
                                        -> [(Int, Int)] 
                                        -> m (Int, Int)
biasedNeighSelection _ _ [] = error "biasedNeighSelction: empty neighbor list"
biasedNeighSelection r (x1, x2) neighs 
    | r > 1 || r < 0 = error "biasedNeighSelection: probability greater than one"
    | otherwise = do
      -- Generate a random number between 0 and 1. If the value is less than
      -- the probability of selecting a horizontal edge, then we select
      -- a horizontal edge. Otherwise, select a vertical edge.
      selHorz <- fmap (<= r) $ getRandomR (0.0, 1.0)
      -- Divide the neighbors into those which are horizontal and those which
      -- are vertical
      let hns = filter (horzNeighbor (x1, x2)) neighs
      let vns = filter (vertNeighbor (x1, x2)) neighs
      -- This is an ugly block of if statements but what it does is first try
      -- to select based on selHorz. Otherwise, in case the neighbors list is
      -- empty in the chosen direction, attempt to use the other direction
      if selHorz && (not . null $ hns)
        --then do 
        --        pos <- getRandomR (0, (length hns) - 1)
        --        return $ hns !! pos
        then selRandList hns
        else if (not selHorz) && (not . null $ vns)
          --then do
          --        pos <- getRandomR (0, (length vns) - 1)
          --        return $ vns !! pos
          then selRandList vns 
        else if (not . null $ hns)
          then selRandList hns
        else if (not . null $ vns)
          then selRandList vns
        else error $ "biasedNeighSelection: horz and vert neighs empty"
                      ++ "\n\tPoint: " ++ (show (x1, x2))
                      ++ "\n\tLength vns: " ++ (show . length $ vns)
                      ++ "\n\tLength hns: " ++ (show . length $ hns)
                      ++ "\n\tselHorz: " ++ (show selHorz)
            where selRandList as = do
                    pos <- getRandomR (0, (length as) - 1)
                    return $ as !! pos

-- Return true if p2 is immediatly above or below p1
vertNeighbor :: (Int, Int) -> (Int, Int) -> Bool
vertNeighbor p1 p2 = case getNeighborDirection p1 p2 of
                       Just North -> True
                       Just South -> True
                       _ -> False

-- Return true if p2 is immediatly left or right wrt p1
horzNeighbor :: (Int, Int) -> (Int, Int) -> Bool
horzNeighbor p1 p2 = case getNeighborDirection p1 p2 of
                       Just East -> True
                       Just West -> True
                       _ -> False

-- Given the passed Grid, update the block at the passed point such that it is
-- open in the passed direction. Also update the block on the otherside  of the
-- newly created opening to reflect the change.
--
-- This will crash if the neighbor point is out-of-bounds (i.e., a path is
-- being opened outside the grid)
openPointOnGrid :: GridDesc -> (Int, Int) -> CardDir -> GridDesc
openPointOnGrid grid pt dir = 
    case neigh of 
      Just n -> openPointOnGrid' (openPointOnGrid' grid pt dir)
                  n (revDir dir)
      Nothing -> error "openPointOnGrid: neighbor out-of-bounds"
    where neigh = getNeighInDir grid dir pt

-- Given the pased Grid, update the block at the passed point such that it is
-- open in the passed direction. 
openPointOnGrid' :: GridDesc -> (Int, Int) -> CardDir -> GridDesc
openPointOnGrid' grid pt dir = grid {blocks = newBlks}
  where blks = blocks grid
        oldPtV = blks M.! pt
        newPtV = openInDir dir oldPtV
        newBlks = M.insert pt newPtV blks

-- Return True if the passed point is open in the passed direction. 
--
-- This will crash if the point is out of bounds
gridOpenInDir :: GridDesc -> (Int, Int) -> CardDir -> Bool
gridOpenInDir gr pt dir = isOpenInDir dir od 
    where blks = blocks gr 
          od = blks M.! pt

isOpenInDir :: CardDir -> OpenDir -> Bool
isOpenInDir North od = northOpen $ openDirToBlock od  
isOpenInDir South od = southOpen $ openDirToBlock od  
isOpenInDir East od  = eastOpen $ openDirToBlock od  
isOpenInDir West od  = westOpen $ openDirToBlock od  

-- Return the passed OpenDir such that it is open in the passed CardDir
openInDir :: CardDir -> OpenDir -> OpenDir
-- Since it is much easier to perform this operation on blocks, do the
-- conversion to and from a block.
openInDir cd od = blockToOpenDir . openBlockInDir cd $ openDirToBlock od

-- Given a block, open it in the passed direction
openBlockInDir :: CardDir -> Block -> Block
openBlockInDir North b = b { northOpen = True}
openBlockInDir South b = b { southOpen = True}
openBlockInDir East b = b { eastOpen = True}
openBlockInDir West b = b { westOpen = True}

-- Given a cardinal direction, return the one on the opposite side (e.g., North
-- --> South)
revDir :: CardDir -> CardDir
revDir North = South
revDir South = North
revDir East = West
revDir West = East

-- Given a point on the grid, and length in the x and y direction, return
-- all the points within the x and y lengths (including the passed point)
getPointsWithin :: (Int, Int) -- Point on grid
                   -> Int     -- X length 
                   -> Int     -- Y length 
                   -> GridDesc -- Grid to examine
                   -> [(Int, Int)]
getPointsWithin (posX, posY) xLen yLen gr = do
    -- Any value from -xLen to xLen may be added to the x-coordinate
    x <- pure (+) <*> [-xLen .. xLen] <*> pure posX
    -- And similarly for the y coordinate
    y <- pure (+) <*> [-yLen .. yLen] <*> pure posY
    -- The point must be inbounds 
    guard $ pointInBounds gr (x, y) 
    return (x, y)

-- Convert the passed grid to an ascii map
toASCII :: GridDesc -> String
toASCII grd = prefix ++ toASCII' grd (h - 1) "" ++ bottomLine
    where prefix = "Height: " ++ (show h) ++ "\n"
          h = gridHeight grd
          w = gridWidth grd
          bottomLine = concat . replicate w $ "+-+"

toASCII' :: GridDesc -> Int -> String -> String
toASCII' grd height s
  | height == -1 = s
  -- | otherwise = toASCII' grd (height - 1)  $ s ++ (toASCIIHorz grd height)
  --                                             ++ (toASCIIVert grd height)
  | otherwise = toASCII' grd (height - 1) nextStr
    where horzStr = toASCIIHorz grd height
          vertStr = toASCIIVert grd height
          nextStr = s ++ horzStr ++ vertStr

toASCIIHorz :: GridDesc ->  Int -> String
toASCIIHorz g height = toASCIIHorz' g 0 height ""

toASCIIHorz' :: GridDesc -> Int -> Int -> String -> String
toASCIIHorz' gr x y s 
    | x >= w = (s ++ "\n")
    | otherwise = toASCIIHorz' gr (x + 1) y nextStr
    where leftOpenChar = if x == 0 then "+" else " "
          rightOpenChar = if x == (w - 1) then "+" else " "
          nextStr = if gridOpenInDir gr (x, y) North
                          then  s ++ leftOpenChar ++ " " ++ rightOpenChar
                          else  s ++ "+-+"
          w = gridWidth gr

toASCIIVert :: GridDesc -> Int -> String
toASCIIVert gr height = toASCIIVert' gr 0 height ""

toASCIIVert' :: GridDesc -> Int -> Int -> String -> String
toASCIIVert' gr x y s 
    | x >= w = (s ++ "\n")
    | otherwise = toASCIIVert' gr (x + 1) y nextStr
    where eastStr = if gridOpenInDir gr (x, y) West
                          then  "  " 
                          else  "| "
          westStr = if gridOpenInDir gr (x, y) East
                          then " "
                          else "|"
          nextStr = s ++ eastStr ++ westStr
          w = gridWidth gr

-- Some pre-created grids for testing
testGrid1 :: GridDesc 
testGrid1 = GridDesc {blocks = M.fromList [((0,0),Open'N)
                                        ,((0,1),Open'NSE)
                                        ,((0,2),Open'NS)
                                        ,((0,3),Open'NSE)
                                        ,((0,4),Open'NS)
                                        ,((0,5),Open'NS)
                                        ,((0,6),Open'SE)
                                        ,((0,7),Open'NE)
                                        ,((0,8),Open'NSE)
                                        ,((0,9),Open'SE)
                                        ,((1,0),Open'NE)
                                        ,((1,1),Open'SW)
                                        ,((1,2),Open'N)
                                        ,((1,3),Open'NSW)
                                        ,((1,4),Open'S)
                                        ,((1,5),Open'NE)
                                        ,((1,6),Open'SW)
                                        ,((1,7),Open'W)
                                        ,((1,8),Open'EW)
                                        ,((1,9),Open'EW)
                                        ,((2,0),Open'NEW)
                                        ,((2,1),Open'NS)
                                        ,((2,2),Open'NS)
                                        ,((2,3),Open'NS)
                                        ,((2,4),Open'SE)
                                        ,((2,5),Open'NW)
                                        ,((2,6),Open'NS)
                                        ,((2,7),Open'NS)
                                        ,((2,8),Open'SW)
                                        ,((2,9),Open'EW)
                                        ,((3,0),Open'EW)
                                        ,((3,1),Open'N)
                                        ,((3,2),Open'SE)
                                        ,((3,3),Open'NE)
                                        ,((3,4),Open'NSW)
                                        ,((3,5),Open'NSE)
                                        ,((3,6),Open'NS)
                                        ,((3,7),Open'S)
                                        ,((3,8),Open'NE)
                                        ,((3,9),Open'SW)
                                        ,((4,0),Open'EW)
                                        ,((4,1),Open'NE)
                                        ,((4,2),Open'SW)
                                        ,((4,3),Open'NW)
                                        ,((4,4),Open'SE)
                                        ,((4,5),Open'EW)
                                        ,((4,6),Open'NE)
                                        ,((4,7),Open'NS)
                                        ,((4,8),Open'SW)
                                        ,((4,9),Open'E)
                                        ,((5,0),Open'EW)
                                        ,((5,1),Open'NW)
                                        ,((5,2),Open'SE)
                                        ,((5,3),Open'NE)
                                        ,((5,4),Open'SW)
                                        ,((5,5),Open'EW)
                                        ,((5,6),Open'NW)
                                        ,((5,7),Open'NS)
                                        ,((5,8),Open'NS)
                                        ,((5,9),Open'SEW)
                                        ,((6,0),Open'NW)
                                        ,((6,1),Open'NS)
                                        ,((6,2),Open'SEW)
                                        ,((6,3),Open'NW)
                                        ,((6,4),Open'SE)
                                        ,((6,5),Open'NEW)
                                        ,((6,6),Open'NS)
                                        ,((6,7),Open'NS)
                                        ,((6,8),Open'S)
                                        ,((6,9),Open'EW)
                                        ,((7,0),Open'NE)
                                        ,((7,1),Open'S)
                                        ,((7,2),Open'EW)
                                        ,((7,3),Open'E)
                                        ,((7,4),Open'EW)
                                        ,((7,5),Open'W)
                                        ,((7,6),Open'NE)
                                        ,((7,7),Open'SE)
                                        ,((7,8),Open'NE)
                                        ,((7,9),Open'SW)
                                        ,((8,0),Open'NEW)
                                        ,((8,1),Open'SE)
                                        ,((8,2),Open'EW)
                                        ,((8,3),Open'EW)
                                        ,((8,4),Open'NW)
                                        ,((8,5),Open'NS)
                                        ,((8,6),Open'SW)
                                        ,((8,7),Open'EW)
                                        ,((8,8),Open'EW)
                                        ,((8,9),Open'E)
                                        ,((9,0),Open'W)
                                        ,((9,1),Open'NW)
                                        ,((9,2),Open'NSW)
                                        ,((9,3),Open'NSW)
                                        ,((9,4),Open'NS)
                                        ,((9,5),Open'NS)
                                        ,((9,6),Open'S)
                                        ,((9,7),Open'NW)
                                        ,((9,8),Open'NSW)
                                        ,((9,9),Open'SW)]
                  
                , gridWidth = 10
                , gridHeight = 10}

testGrid2 :: GridDesc
testGrid2 = GridDesc {blocks = M.fromList [((0,0),Open'NE)
                                          ,((0,1),Open'NS)
                                          ,((0,2),Open'NS)
                                          ,((0,3),Open'SE)
                                          ,((1,0),Open'NEW)
                                          ,((1,1),Open'S)
                                          ,((1,2),Open'NE)
                                          ,((1,3),Open'SEW)
                                          ,((2,0),Open'NEW)
                                          ,((2,1),Open'SE)
                                          ,((2,2),Open'EW)
                                          ,((2,3),Open'EW)
                                          ,((3,0),Open'W)
                                          ,((3,1),Open'NW)
                                          ,((3,2),Open'SW)
                                          ,((3,3),Open'W)
                                          ]
                                          , gridWidth = 4
                                          , gridHeight = 4}
