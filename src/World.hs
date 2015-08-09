-- Author: Markus Kusano
--
-- World is a record containing the level environment as well as all the
-- stuff inside of it
module World (World
             , levelGrid
             , playerPos
             , worldWidth
             , worldHeight
             , fromGridDesc
             , fromGridDescWithPlPos
             , worldToPicture
             , worldTilesToPicture
             , updatePlayerPos
             , inWall
             ) where

import Level (GridDesc
             , gridWidth
             , gridHeight
             , CardDir (..)
             , blocks
             , OpenDir 
             , getPointsAndDirWithin
             , blocksToPic2
             , openDirToBlock
             , Block
             , northOpen
             , southOpen
             , eastOpen
             , westOpen
             )

import Data.Monoid ((<>))
import qualified Graphics.Gloss as Gloss
import qualified Data.Map as M

-- The length of one wall of a block (from GridDesc) in terms of tiles
tilesPerBlock :: Int
tilesPerBlock = 10

-- The entire world is made up of tiles which are either solid or empty
data Tile = Solid | Empty
          deriving Show

data World = World { 
                     -- The  layout of all the blocks making up the level
                     levelGrid :: GridDesc 
                     -- Position of the player in world coordinates
                   , playerPos :: (Double, Double)
                   , worldWidth :: Double
                   , worldHeight :: Double
                   -- A mapping from each tile-level coordinate to its
                   -- tile-state
                   , tiles :: M.Map (Int, Int) Tile
                   }

-- Update the position of the player, scootching if it is colliding with
-- something
updatePlayerPos :: World -> (Double, Double) -> World
updatePlayerPos w p@(x, y) = w { playerPos = (x + xCor, y + yCor) }
  where -- get how much, if any, the point is in the wall
        (mayXCor, mayYCor) = inWall w p
        xCor = case mayXCor of
                 Nothing -> 0
                 Just d -> d
        yCor = case mayYCor of
                 Nothing -> 0
                 Just d -> d
        

-- Return true the distance in the x and y direction that the passed point is
-- in the wall in the passed world. Nothing indicates the point is not in the
-- wall in that direction
--
-- This will crash if the point is out-of-bounds
inWall :: World -> (Double, Double) -> (Maybe Double, Maybe Double)
inWall w p@(x, y) = case mayBl of
                    Nothing -> error $ "[ERROR] inWall: out of bounds: " 
                                       ++ (show p)
                    Just bl -> inWallBlock (openDirToBlock bl) p
  where blockX = floor x
        blockY = floor y
        bls = blocks . levelGrid $ w
        mayBl = M.lookup (blockX, blockY) bls

-- Given a block in the world which is assumed to be the block where the passed
-- return the ammount in the x and y direction that the point is inside of
-- a wall. 
inWallBlock :: Block -> (Double, Double) -> (Maybe Double, Maybe Double)
inWallBlock b (x, y) = (eastWestAdj, northSouthAdj)
    where tol = 0.05
          -- Floor/Ceiling correspond to the rounded part of the cooridnates
          -- or, quite intuitively, the ceiling or wall of the room (blocks
          -- always have a width and height of one)
          -- First, calculate the distance of the point from the walls
          northDist  = (abs (y - (doubleCeiling y)))
          southDist = (abs (y - (doubleFloor y)))
          eastDist = (abs (x - (doubleCeiling x)))
          westDist = (abs (x - (doubleFloor x)))
          -- Adjustments to the players position to get him/her out of the wall
          --
          -- We need to correct the position if it is inside the wall. The
          -- amount the object is inside the wall is the difference between
          -- the tolerance and the distance
          northSouthAdj = if (northDist < tol) && (not $ northOpen b)
                            then Just . negate . abs $ northDist - tol
                            else if (southDist < tol) && (not $ southOpen b)
                              then Just . abs $ southDist - tol
                            else Nothing
          eastWestAdj = if (eastDist < tol) && (not $ eastOpen b)
                            then Just . negate . abs $ eastDist - tol
                            else if (westDist < tol) && (not $ westOpen b)
                              then Just . abs $ westDist - tol
                            else Nothing
          doubleCeiling :: Double -> Double 
          doubleCeiling d = fromIntegral ((ceiling d) :: Int)
          doubleFloor :: Double -> Double 
          doubleFloor d = fromIntegral ((floor $ d) :: Int)

-- Create a World from a grid description.
--
-- The coordinate system of the world is setup such that each block in the
-- grid is a one-by-one square in the world. Since the world cooridnates
-- are floating points, this allows for "arbitrary" positioning of stuff
-- within each block.
fromGridDesc :: GridDesc -> World
fromGridDesc gr = World { levelGrid = gr
                        , playerPos = (0.0, 0.0)
                        , worldWidth = fromIntegral $ gridWidth gr
                        , worldHeight = fromIntegral $ gridHeight gr
                        , tiles = tilesFromGrid gr
                        }

-- Same as fromGridDesc but includes an initial player position
fromGridDescWithPlPos :: GridDesc -> (Double, Double) -> World
fromGridDescWithPlPos gr pos = w { playerPos = pos }
    where w = fromGridDesc gr

-- Given a grid description, enumerate all the tiles in the world. This
-- creates solid blocks for all the walls
tilesFromGrid :: GridDesc -> M.Map (Int, Int) Tile
tilesFromGrid gr = M.foldrWithKey blockPointToTiles M.empty bls
  where bls :: M.Map (Int, Int) Block
        bls = M.map (openDirToBlock) (blocks gr)

-- Given a block and it associated point in the grid return and the current
-- tile map, update the tile map to reflect the addition of the current
-- block
blockPointToTiles :: (Int, Int) 
                     -> Block 
                     -> M.Map (Int, Int) Tile 
                     -> M.Map (Int, Int) Tile
blockPointToTiles (bx, by) bl m = M.union m tileBlk
  where -- Each block is represented with a single quardinant. Inside this
        -- block, are a bunch of tiles. So, first convert the block based
        -- coordinates to tile coordinates. This is simply based on the length
        -- of each wall of the block. These two coordinates are the lower left
        -- corner of the block in tile coordinates
        xTilePos = bx * tilesPerBlock
        yTilePos = by * tilesPerBlock
        tileBlk = tilesFromBlock xTilePos yTilePos bl 

-- Given a block, enumerate its walls into tiles. The Two Int arguments are
-- offsets for the X and Y directions respectively.
tilesFromBlock :: Int -> Int -> Block -> M.Map (Int, Int) Tile
-- Note: M.union perfers items on the left. So, any items in the wall map is
-- preserved while empty tiles are overwritten
tilesFromBlock xOff yOff b = M.union wallTiles es
  where northWall = if (not . northOpen $ b)
                      then getWallPoints xOff yOff North tilesPerBlock
                      else []
        southWall = if (not . southOpen $ b)
                      then getWallPoints xOff yOff South tilesPerBlock
                      else []
        eastWall = if (not . eastOpen $ b)
                      then getWallPoints xOff yOff East tilesPerBlock
                      else []
        westWall = if (not . westOpen $ b)
                      then getWallPoints xOff yOff West tilesPerBlock
                      else []
        wallPoints = northWall <> southWall <> eastWall <> westWall
        wallTiles = M.fromList $ zip wallPoints (repeat Solid)
        es = emptyTiles xOff yOff

-- Given x and y offsets, create a map of all empty tiles
emptyTiles :: Int -> Int -> M.Map (Int, Int) Tile
emptyTiles xOff yOff = M.fromList (zip ts (repeat Empty))
    where ts = do
                  x <- [xOff .. xOff + tilesPerBlock]
                  y <- [yOff .. yOff + tilesPerBlock]
                  return (x, y)


-- Given the number of tiles to use per wall of a block, return a list of
-- tile-points representing the wall of a block in a certain cardinal
-- direction. The points will be based off the lower-left corner of the
-- block being (0,0).
--
-- The first two Int arguments are offsets added in the X or Y direction
getWallPoints :: Int -> Int -> CardDir -> Int -> [(Int, Int)]
getWallPoints xOff yOff dir sz = 
    case dir of
      North -> do
                  x <- [xStart .. xEnd]
                  y <- [yEnd]
                  return (x, y)
      South -> do
                  x <- [xStart .. xEnd]
                  y <- [yStart]
                  return (x, y)
      East -> do
                  x <- [xEnd]
                  y <- [yStart .. yEnd]
                  return (x, y)
      West -> do 
                  x <- [xStart]
                  y <- [yStart .. yEnd]
                  return (x, y)
  where xStart = xOff
        -- Need to subtract one since points are zero indexed
        xEnd = sz + xOff - 1
        yStart = yOff
        yEnd = sz + yOff - 1

-- Given a map of all the tiles in the world, render the block at the passed
-- coordinates. The coordinates of the block should be in block (not tile)
-- based coordinates
blockToPicTiles :: (Int, Int) -- Block width and height relative to framebuffer
                   -> M.Map (Int, Int) Tile -- Tile map
                   -> (Int, Int) -- Block position to render
                   -> Gloss.Picture
blockToPicTiles (bW, bH) tm (xB, yB) = 
    mconcat $ map (renderSolidTile tileWidth tileHeight) (map tileToFB solidCoords)
  where xT = xB * tilesPerBlock
        yT = yB * tilesPerBlock
        tileWidth = (fromIntegral bW) / (fromIntegral tilesPerBlock)
        tileHeight = (fromIntegral bH) / (fromIntegral tilesPerBlock)
        -- Tile coordinates suitable for looking up in the world's tilemap
        tileCoords :: [(Int, Int)]
        tileCoords = do
                        x <- take tilesPerBlock $ [xT..]
                        y <- take tilesPerBlock $ [yT..]
                        return (x, y)
        solidCoords = filter (solid tm) tileCoords
        tileToFB :: (Int, Int) -> (Float, Float)
        tileToFB (x, y) = ( (fromIntegral x) * tileWidth
                          , (fromIntegral y) * tileHeight)
        solid :: M.Map (Int, Int) Tile -> (Int, Int) -> Bool
        solid m p = case M.lookup p m of
                     Nothing -> error $ "[ERROR] blockToPicTiles: tile not "
                                        ++ "found in world " ++ (show p)
                     Just Empty -> False
                     Just Solid -> True
          

-- Render a title given its x and y position. The lower-left corner
-- will be at (0,0). The first two int arguments are the tile width and height
-- in framebuffer terms
renderSolidTile :: Float -> Float -> (Float, Float) -> Gloss.Picture
renderSolidTile tW tH (xPos, yPos) = Gloss.color 
                             Gloss.black 
                             (Gloss.polygon [(xPos, yPos)
                                           , (xPos + tW,  yPos)
                                           , (xPos + tW, yPos + tH)
                                           , (xPos, yPos + tH)])
    where -- xPosF = (fromIntegral xPos) :: Float
          -- yPosF = (fromIntegral yPos) :: Float
          --tWF = (fromIntegral tW) :: Float
          --tHF = (fromIntegral tH) :: Float

blocksToPicTiles :: (Int, Int) -- Block width and height relative to framebuffer
                   -> M.Map (Int, Int) Tile -- Tile map
                   -> [(Int, Int)] -- Block positions to render
                   -> Gloss.Picture
blocksToPicTiles sz tm bls = mconcat $ map (blockToPicTiles sz tm) bls


-- Render the tiles of the world 
--worldTilesToPicture :: Int      -- Block height in framebuffer pixels
worldTilesToPicture :: (Int, Int)      -- Framebuffer size
                  -> (Int, Int) -- Size of box around playerPos in world
                                -- coordinates to draw. Everything within
                                -- this box will be rendered within the
                                -- framebuffer size. Effectively, this
                                -- changes the "zoom level"
                  -> World
                  -> Gloss.Picture
worldTilesToPicture (fbW, fbH) (wW, wH) world = picTrans <> player
--worldTilesToPicture fbH (wW, wH) world = picTrans <> player
  where -- Width of a block relative to the framebuffer. This depends on how
        -- large an area the user wants to view. Take the ceiling so we always
        -- over-draw and chop when rendering
        fbBlockWidth :: Int
        fbBlockWidth = ceiling $ ((fromIntegral fbW) :: Double) 
                                  / ((fromIntegral wW) :: Double)
        fbBlockHeight :: Int
        fbBlockHeight = ceiling $ ((fromIntegral fbH) :: Double) 
                                  / ((fromIntegral wH) :: Double)
        blockLength :: Int 
        blockLength = maximum [fbBlockWidth, fbBlockHeight]
        -- Everything is a square so width equals height
        --fbBlockWidth = fbBlockHeight
        -- The length from the player in the X direction which we will render.
        -- Again, we take the ceiling so that we always over-render the scene
        -- rather than under render. We divide the box length by two since it
        -- is a box around the player: at most, we can go half the length from
        -- the player
        xLen :: Int
        xLen = ceiling $ ((fromIntegral wW) :: Double) / 2.0
        yLen :: Int
        yLen = ceiling $ ((fromIntegral wH) :: Double) / 2.0
        -- Blocks from the world which may be visible. Take the floor of the
        -- player position to "round it down" to the current block that the
        -- player is within. For instance, if the position is (1.4, 3.2) then
        -- the player is somewhere within the block at (1, 3).
        occludedBlocks :: M.Map (Int, Int) OpenDir
        occludedBlocks = getPointsAndDirWithin (doublePairFloor $ playerPos world) 
                                               xLen 
                                               yLen 
                                               (levelGrid world)
        doublePairFloor :: (Double, Double) -> (Int, Int)
        doublePairFloor (d1, d2) = (floor d1, floor d2)
        --pic = blocksToPicTiles (fbBlockWidth, fbBlockHeight) 
        pic = blocksToPicTiles (blockLength, blockLength) 
                               (tiles world) 
                               (M.keys occludedBlocks)
        -- Draw a circle for the player
        --player = Gloss.circle ((fromIntegral fbBlockWidth) * (0.1))
        player = Gloss.circle ((fromIntegral blockLength) * (0.1))
        -- Translate the picture so it is centered on the player. This requires
        -- translating the players current position into framebuffer
        -- cooridnates and then translating
        --plFBX = (fst . playerPos $ world) * (fromIntegral fbBlockWidth)
        --plFBY = (snd . playerPos $ world) * (fromIntegral fbBlockHeight)
        plFBX = (fst . playerPos $ world) * (fromIntegral blockLength)
        plFBY = (snd . playerPos $ world) * (fromIntegral blockLength)
        picTrans = Gloss.Translate --(negate . realToFrac . fst $ playerPos world) 
                                   --(negate . realToFrac . snd $ playerPos world) 
                                   (realToFrac . negate $ plFBX) 
                                   (realToFrac . negate $ plFBY)
                                   pic

-- Render the walls in the World as vector lines.
--worldToPicture :: (Int, Int) -- Size of framebuffer
worldToPicture :: Int           -- block width in framebuffer pixels
                  -> (Int, Int) -- Size of box around playerPos in world
                                -- coordinates to draw. Everything within
                                -- this box will be rendered within the
                                -- framebuffer size. Effectively, this
                                -- changes the "zoom level"
                  -> World
                  -> Gloss.Picture
--worldToPicture (fbW, fbH) (wW, wH) world = picTrans <> player
worldToPicture fbH (wW, wH) world = picTrans <> player
  where -- The width of a block in framebuffer coordinates (pixels). The block
        -- width depends on the size of the world to map to the framebuffer
        -- (`(w,h)`). For example, if it is (1,1) then we render a box in world
        -- coordinates of size one which maps to the size of one block. If it
        -- is larger than one, then we want to render more than one block so
        -- the width of one block should be smaller than the width of the
        -- entire framebuffer. All in all: just divide. We take the ceiling so
        -- that if we always render a little extra and not a little less. When
        -- drawing on the framebuffer, information can always be off the
        -- screen.
        --fbBlockWidth :: Int
        --fbBlockWidth = ceiling $ ((fromIntegral fbW) :: Double) 
        --                          / ((fromIntegral wW) :: Double)
        fbBlockHeight :: Int
        fbBlockHeight = ceiling $ ((fromIntegral fbH) :: Double) 
                                  / ((fromIntegral wH) :: Double)
        fbBlockWidth = fbBlockHeight
        -- The length from the player in the X direction which we will render.
        -- Again, we take the ceiling so that we always over-render the scene
        -- rather than under render. We divide the box length by two since it
        -- is a box around the player: at most, we can go half the length from
        -- the player
        xLen :: Int
        xLen = ceiling $ ((fromIntegral wW) :: Double) / 2.0
        yLen :: Int
        yLen = ceiling $ ((fromIntegral wH) :: Double) / 2.0
        -- Blocks from the world which may be visible. Take the floor of the
        -- player position to "round it down" to the current block that the
        -- player is within. For instance, if the position is (1.4, 3.2) then
        -- the player is somewhere within the block at (1, 3).
        occludedBlocks :: M.Map (Int, Int) OpenDir
        occludedBlocks = getPointsAndDirWithin (doublePairFloor $ playerPos world) 
                                               xLen 
                                               yLen 
                                               (levelGrid world)
        doublePairFloor :: (Double, Double) -> (Int, Int)
        doublePairFloor (d1, d2) = (floor d1, floor d2)
        -- Draw all the blocks with the bottom left corner at (0,)
        pic = blocksToPic2 (fbBlockWidth, fbBlockHeight) 
                           occludedBlocks
        -- Draw a circle for the player
        player = Gloss.circle ((fromIntegral fbBlockWidth) * (0.1))
        -- Translate the picture so it is centered on the player. This requires
        -- translating the players current position into framebuffer
        -- cooridnates and then translating
        plFBX = (fst . playerPos $ world) * (fromIntegral fbBlockWidth)
        plFBY = (snd . playerPos $ world) * (fromIntegral fbBlockHeight)
        picTrans = Gloss.Translate --(negate . realToFrac . fst $ playerPos world) 
                                   --(negate . realToFrac . snd $ playerPos world) 
                                   (realToFrac . negate $ plFBX) 
                                   (realToFrac . negate $ plFBY)
                                   pic

