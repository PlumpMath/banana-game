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
             ) where

import Level (GridDesc
             , gridWidth
             ,gridHeight
             )
import qualified Graphics.Gloss as Gloss


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

-- Create 
worldToPicture :: (Int, Int) -- Size of framebuffer
                  -> (Int, Int) -- Size of box around playerPos in world
                                -- coordinates to draw. Everything within
                                -- this box will be rendered within the
                                -- framebuffer size. Effectively, this
                                -- changes the "zoom level"
                  -> World
                  -> Gloss.Picture
worldToPicture (w, h) world = undefined
