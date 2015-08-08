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
             ) where

import Level (GridDesc
             , gridWidth
             , gridHeight
             , OpenDir 
             , getPointsAndDirWithin
             , blocksToPic2
             )

import Data.Monoid ((<>))
import qualified Graphics.Gloss as Gloss
import qualified Data.Map as M


data World = World { 
                     -- The  layout of all the blocks making up the level
                     levelGrid :: GridDesc 
                     -- Position of the player in world coordinates
                   , playerPos :: (Double, Double)
                   , worldWidth :: Double
                   , worldHeight :: Double
                   }

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
                        }

-- Same as fromGridDesc but includes an initial player position
fromGridDescWithPlPos :: GridDesc -> (Double, Double) -> World
fromGridDescWithPlPos gr pos = w { playerPos = pos }
    where w = fromGridDesc gr

-- Create 
worldToPicture :: (Int, Int) -- Size of framebuffer
                  -> (Int, Int) -- Size of box around playerPos in world
                                -- coordinates to draw. Everything within
                                -- this box will be rendered within the
                                -- framebuffer size. Effectively, this
                                -- changes the "zoom level"
                  -> World
                  -> Gloss.Picture
worldToPicture (fbW, fbH) (wW, wH) world = picTrans <> player
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
        fbBlockWidth :: Int
        fbBlockWidth = ceiling $ ((fromIntegral fbW) :: Double) 
                                  / ((fromIntegral wW) :: Double)
        fbBlockHeight :: Int
        fbBlockHeight = ceiling $ ((fromIntegral fbH) :: Double) 
                                  / ((fromIntegral wH) :: Double)
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

