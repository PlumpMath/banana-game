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
             , updatePlayerPosTile
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
--data Tile = Solid | Empty
--          deriving Show

-- A tile can be clipped on in any of the cardinal directions. If it cannot
-- be clipped in a certain direction, then it can be passed through in
-- that direction
data Tile = Tile { clipWest :: Bool
                 , clipEast :: Bool
                 , clipNorth :: Bool
                 , clipSouth :: Bool }

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

tileIsEmpty :: Tile -> Bool
tileIsEmpty (Tile False False False False) = True
tileIsEmpty _ = False

-- A tile which can clip on all slides
solidTile :: Tile
solidTile = Tile True True True True

-- A tile which can be passed through on all slides
emptyTile :: Tile
emptyTile = Tile False False False False

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

-- Update the players position to the passed pair assuming the player has
-- the passed width and height (assuming the point is the center of
-- a rectangle representing the player).
updatePlayerPosTile :: World -> Double -> Double -> (Double, Double) -> World
updatePlayerPosTile world w h p@(x,y) = 
    case M.size solidOverlap of
      0 -> world { playerPos = p }
      _ -> world { playerPos = (xFixed, yFixed) }
            where ts :: [((Int, Int), Tile)]
                  ts = M.toList solidOverlap
                  -- The tiles overlapping with the point in world coordinates
                  clipTiles :: [((Double, Double, Double, Double), Tile)]
                  clipTiles = map tileToWorldRect2 ts
                  -- Fixed pos returns the width and height of the player
                  -- along with its location
                  (xFixed, yFixed, _, _) = movePointsClip (x, y, w, h) clipTiles
  where -- Get all the tiles overlapping with the point
        solidOverlap = M.filter (not . tileIsEmpty) (onTiles world w h p)

-- Same as tileWorldToRect but preserves the second item in the
-- pair
tileToWorldRect2 :: ((Int, Int), a) 
                    -> ((Double, Double, Double, Double), a)
tileToWorldRect2 (p, sn) = ((x, y, w, h), sn)
  where (x, y, w, h) = tileToWorldRect p

-- Given the location of a tile in tile coordinates, return its
-- center, width, and height in world coordinates
tileToWorldRect :: (Int, Int) -> (Double, Double, Double, Double)
tileToWorldRect (x, y) = (worldX, worldY, tileLength, tileLength)
  where worldX = (fromIntegral x) / (fromIntegral tilesPerBlock)
        worldY = (fromIntegral y) / (fromIntegral tilesPerBlock)
        tileLength :: Double
        tileLength = 1.0 / (fromIntegral tilesPerBlock)



-- Return the amount the passed point, in world coordinates, is inside of
-- any tile in the x and y directions. The aditional double arguments are
-- the width and height of the object (in world coordinates) centered at the passed point
--inTile :: World -> Double -> Double -> (Double, Double) -> (Maybe Double, Maybe Double)
--inTile world w h p@(x, y) = undefined
--  where 
--        -- Convert world coordinates to tile coordinates. Take the floor to
--        -- get the 
--        xT = x * (fromIntegral tilesPerBlock)
--        yT = x * (fromIntegral tilesPerBlock)
--        -- The maximum distance in the x direction from the point which the
--        -- object can be in 
--        xMaxT = xT + (w / 2)
--        yMaxT = yT + (h / 2)
--        -- Get any point within the tile the passed point is in up to any
--        -- tile within reach of the object. Over-approximate by taking the
--        -- floor of the current position and ceiling of the objects "reach"
--        reachable :: [(Int, Int)]
--        reachable = do
--                      xReach <- [(floor xT) .. (ceiling xMaxT)]
--                      yReach <- [(floor yT) .. (ceiling yMaxT)]
--                      return (xReach, yReach)
--        -- Use the tile map to determine if the coordinates are solid
--        solidTs = filter (solid . tiles $ world) reachable
--        adjAmount :: (Maybe Double, Maybe Double)
--        adjAmount = case solidTs of 
--                      [] -> (Nothing, Nothing)
--                      l ->  -- Get the closest point to calculate the
--                            -- smallest amount to scoot the point to make
--                            -- it inbounds
--                            let minPt = foldr (minXYDist p) (head solidTs) (tail solidTs)
--        -- Compare `p` to `c`. If `c` is closer to `p` than `m`, then
--        -- return `c`, otherwise, return `m`
--        minXYDist :: (Double, Double) -> (Int, Int) -> (Int, Int) -> (Int, Int)
--        minXYDist b@(bX, bY) c@(cX, cY) m@(mX, mY) = 
--            case distC < distM of 
--                True -> c
--                False -> m
--          where distC = distance b c
--                distM = distance b m 
--                distance :: (Double, Double) -> (Int, Int) -> Double
--                distance (axD, ayD) (bx, by) = sqrt (xDist + yDist)
--                  where 
--                        --axD :: Double
--                        --axD = fromIntegral ax
--                        --ayD :: Double
--                        --ayD = fromIntegral ay
--                        bxD :: Double
--                        bxD = fromIntegral bx
--                        byD :: Double
--                        byD = fromIntegral by
--                        xDist :: Double
--                        xDist = (bxD - axD) ** 2
--                        yDist = (byD - ayD) ** 2

-- Return those tiles overlapping with the passed point. The point is
-- assumed to have a width and height radiating in the passed directions
-- respectively. The distances should be passed in world coordinates
onTiles :: World -> Double -> Double -> (Double, Double) -> M.Map (Int, Int) Tile
onTiles world w h (x, y) = ts
  where 
        -- Convert world coordinates to tile coordinates. Take the floor to
        -- get the 
        xT = x * (fromIntegral tilesPerBlock)
        yT = y * (fromIntegral tilesPerBlock)
        -- The maximum distance in the x direction from the point which the
        -- object can be in 
        xMaxT = xT + (w / 2)
        yMaxT = yT + (h / 2)
        -- Get any point within the tile the passed point is in up to any
        -- tile within reach of the object. Over-approximate by taking the
        -- floor of the current position and ceiling of the objects "reach"
        reachable :: [(Int, Int)]
        reachable = do
                      xReach <- [(floor xT) .. (ceiling xMaxT)]
                      yReach <- [(floor yT) .. (ceiling yMaxT)]
                      return (xReach, yReach)
        -- Lookup all the tiles in the world and return the results
        ts = M.filterWithKey (\k _ -> k `elem` reachable) (tiles world)

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
  where northWallPts = if (not . northOpen $ b)
                      then getWallPoints xOff yOff North tilesPerBlock
                      else []
        southWallPts = if (not . southOpen $ b)
                      then getWallPoints xOff yOff South tilesPerBlock
                      else []
        eastWallPts = if (not . eastOpen $ b)
                      then getWallPoints xOff yOff East tilesPerBlock
                      else []
        westWallPts = if (not . westOpen $ b)
                      then getWallPoints xOff yOff West tilesPerBlock
                      else []
        -- A tile creating a northern wall cannot be passed on the south
        northTile = emptyTile { clipSouth = True }
        southTile = emptyTile { clipNorth = True }
        eastTile = emptyTile { clipWest = True }
        westTile = emptyTile { clipEast = True }
        northTiles = zip northWallPts (repeat northTile)
        southTiles = zip southWallPts (repeat southTile)
        eastTiles = zip eastWallPts (repeat eastTile)
        westTiles = zip westWallPts (repeat westTile)
        wallTiles = M.fromList $ northTiles <> southTiles <> eastTiles 
                                 <> westTiles 
        es = emptyTiles xOff yOff

-- Given x and y offsets, create a map of all empty tiles
emptyTiles :: Int -> Int -> M.Map (Int, Int) Tile
emptyTiles xOff yOff = M.fromList (zip ts (repeat emptyTile))
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
        solidCoords = filter (mayClip tm) tileCoords
        tileToFB :: (Int, Int) -> (Float, Float)
        tileToFB (x, y) = ( (fromIntegral x) * tileWidth
                          , (fromIntegral y) * tileHeight)


-- Return true if the passed coordinate in the passed map is solid
mayClip :: M.Map (Int, Int) Tile -> (Int, Int) -> Bool
mayClip m p = case M.lookup p m of
             Nothing -> error $ "[ERROR] blockToPicTiles: tile not "
                                ++ "found in world " ++ (show p)
             Just t -> not $ tileIsEmpty t
          

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
        player = Gloss.circle ((fromIntegral fbBlockWidth) * (0.03))
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

-- Given a point with a width and height and a tile with a width and
-- height, move the point so it is no longer clipping with the tile
movePointClip :: Tile -> (Double, Double) -> Double -> Double
                 -> (Double, Double, Double, Double)
                 -> (Double, Double)
movePointClip tile p@(xp, yp) pW pH (xt, yt, tW, tH) = (xp + xFix, yp + yFix)
  where tPoint = (xt, yt)
        xFix = if (clipEast tile) || (clipWest tile) 
                then (clipDistEast p pW pH tPoint tW tH)
                     + clipDistWest p pW pH tPoint tW tH
                else 0
        yFix = if (clipNorth tile) || (clipSouth tile)
                then (clipDistNorth p pW pH tPoint tW tH)
                     + clipDistSouth p pW pH tPoint tW tH
                else 0

-- Same as movePointClip but matches the signature of fold
movePointClip2 :: ((Double, Double, Double, Double), Tile) -- solid tile  rectangle
                  -> (Double, Double, Double, Double) -- target rectangle
                  -> (Double, Double, Double, Double) -- Updated target 
movePointClip2 (t, tile) (px, py, pW, pH) = (corPX, corPY, pW, pH)
    where (corPX, corPY) = movePointClip tile (px, py) pW pH t

-- Same as movePointClip but fixes based on a list of tiles. The tiles are
-- fixed in the order they are in the list 
movePointsClip :: (Double, Double, Double, Double)    -- "Player" rectangle 
                  -> [((Double, Double, Double, Double), Tile)] -- Tile rectagle
                  -> (Double, Double, Double, Double) 
movePointsClip p [] = p
movePointsClip p ts = foldr movePointClip2 p ts

-- Given the center of two rectangs along with the width and height of the
-- rectangles return the distance that the north edge of the first needs to
-- be moved in the x and y directions to not be inside the second.
clipDistNorth :: (Double, Double) -> Double -> Double
                -> (Double, Double) -> Double -> Double
                -> Double
clipDistNorth t@(_, yT) tW tH c@(_, yC) cW cH = yFix
  where (neClip, seClip, swClip, nwClip) = clipPoints t tW tH c cW cH
        yDist = abs (yT - yC)
        yFix = if (neClip || nwClip)
                  && (not seClip)
                  && (not swClip)
                then yDist - (0.5 * tH) - (0.5 * cH)
                else 0

clipDistSouth :: (Double, Double) -> Double -> Double
                -> (Double, Double) -> Double -> Double
                -> Double
clipDistSouth t@(_, yT) tW tH c@(_, yC) cW cH = yFix
  where (neClip, seClip, swClip, nwClip) = clipPoints t tW tH c cW cH
        yDist = abs (yT - yC)
        yFix = if (seClip || swClip)
                  && (not neClip)
                  && (not nwClip)
                then negate $ yDist - (0.5 * tH) - (0.5 * cH)
                else 0

clipDistEast :: (Double, Double) -> Double -> Double
                -> (Double, Double) -> Double -> Double
                -> Double
clipDistEast t@(xT, _) tW tH c@(xC, _) cW cH = xFix
  where (neClip, seClip, swClip, nwClip) = clipPoints t tW tH c cW cH
        xDist = abs (xT - xC)
        xFix = if (neClip || seClip)
                  && (not nwClip)
                  && (not swClip)
                then xDist - (0.5 * tW) - (0.5 * cW)
                else 0

clipDistWest :: (Double, Double) -> Double -> Double
                -> (Double, Double) -> Double -> Double
                -> Double
clipDistWest t@(xT, _) tW tH c@(xC, _) cW cH = xFix
  where (neClip, seClip, swClip, nwClip) = clipPoints t tW tH c cW cH
        xDist = abs (xT - xC)
        xFix = if (nwClip || swClip)
                  && (not neClip)
                  && (not seClip)
                then negate $ xDist - (0.5 * tW) - (0.5 * cW)
                else 0

-- Return if the NE, SE, SW, NW corners of the first shape are inside in
-- the second shape
clipPoints :: (Double, Double) -> Double -> Double
                -> (Double, Double) -> Double -> Double
                -> (Bool, Bool, Bool, Bool)
clipPoints (xT, yT) tW tH c cW cH = (neClip, seClip, swClip, nwClip)
  where halfWidth = 0.5 * tW 
        halfHeight = 0.5 * tH
        tNEPoint = (xT + halfWidth
                    , yT + halfHeight)
        tSEPoint = (xT + halfWidth 
                    , yT -  halfHeight)
        tSWPoint = (xT - halfWidth 
                    , yT -  halfHeight)
        tNWPoint = (xT - halfWidth 
                    , yT +  halfHeight)
        --northEdge = (tNWPoint, tNEPoint)
        --southEdge = (tSWPoint, tSEPoint)
        --eastEdge = (tSEPoint, tNEPoint)
        --westEdge = (tSWPoint, tNWPoint)
        neClip = pointInRect tNEPoint c cW cH
        seClip = pointInRect tSEPoint c cW cH 
        swClip = pointInRect tSWPoint c cW cH
        nwClip = pointInRect tNWPoint c cW cH

pointInRect :: (Double, Double)
               -> (Double, Double) -> Double -> Double
               -> Bool
pointInRect (xp, yp) c w h = (xp >= xMin)
                                        && (xp <= xMax)
                                        && (yp >= yMin)
                                        && (yp <= yMax)
  where (nep, sep, swp, nwp) = rectCorners c w h
        xs = [fst nep, fst sep, fst swp, fst nwp]
        ys = [snd nep, snd sep, snd swp, snd nwp]
        xMin = minimum xs
        xMax = maximum xs
        yMin = minimum ys
        yMax = maximum ys

rectCorners :: (Double, Double) -> Double -> Double
               -> ((Double, Double)     -- NE corner
                   , (Double, Double)   -- SE corner
                   , (Double, Double)   -- SW corner
                   , (Double, Double))  -- NW Corner
rectCorners (x, y) w h = (nePoint, sePoint, swPoint, nwPoint)
  where halfWidth = 0.5 * w
        halfHeight = 0.5 * h
        nePoint = (x + halfWidth
                    , y + halfHeight)
        sePoint = (x + halfWidth 
                    , y -  halfHeight)
        swPoint = (x - halfWidth 
                    , y -  halfHeight)
        nwPoint = (x - halfWidth 
                    , y +  halfHeight)

--doubleDist :: (Double, Double) -> (Double, Double) -> Double
--doubleDist (x1, y1) (x2, y2) = sqrt $ xsq + ysq
--  where xsq = (x2 - x1) ** 2
--        ysq = (y2 - y2) ** 2

-- Return true if the passed edge is inside the passed rectangle defined by
-- a center point and a width and height
--edgeInRectangle :: ((Double, Double), (Double, Double))
--                   -> (Double, Double) -> Double -> Double
--                   -> Bool
--edgeInRectagle (ep1, ep2) (rX, rY) rW rH = 
--    if (rX + rW) 

-- Return the direction of the first point relative to the second
--relativeDir :: (Double, Double) -> (Double,  Double) -> CardDir
