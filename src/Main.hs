module Main where
import qualified Graphics.Gloss as Gloss
import qualified Graphics.Gloss.Rendering as Gloss
--import Graphics.Rendering.OpenGL.Raw -- as gl*
--import System.Mem (performGC)
import Control.Monad (when
                     , unless)
import Control.Monad.Random (evalRand
                            , getStdGen)
import Data.Monoid ((<>))
import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL 
import System.Exit (exitFailure
                   , exitSuccess)
import System.IO (hPutStrLn
                 , stderr)

--import Linear.Affine (Point (P))
--import Linear (V2 (V2))
-- import Foreign.Marshal.Alloc (alloca)
--import Foreign.Storable (peek)
import Data.Maybe (fromJust, isNothing)
import qualified Data.Map as M
-- import Data.Text (pack)
--import Control.Applicative ((<$>))
import Reactive.Banana.Frameworks (newAddHandler
                                  , actuate
                                  , compile
                                  , fromAddHandler
                                  , Frameworks
                                  , reactimate
                                  , AddHandler)
import Reactive.Banana.Switch (Moment)
--import qualified Data.Set as S

import Level (GridDesc
             , openDirToBlock
             , OpenDir
             , Block
             , northOpen
             , southOpen
             , eastOpen
             , westOpen
             , gridWidth
             , gridHeight
             , blocks
             , dfsMaze)
import World (World)

-- Compile time option to disable debugging messages 
debugEnable :: Bool
debugEnable = True

die :: String -> IO ()
die s = do 
    hPutStrLn stderr s
    _ <- exitFailure
    return ()

-- Same as die but calls glfw terminate
dieAndTerm :: String -> IO ()
dieAndTerm s = do
    GLFW.terminate
    die s 

debugMsg :: String -> IO ()
debugMsg s = do
    when debugEnable $ hPutStrLn stderr s

-- Called on GFLW error
errorCallback :: GLFW.ErrorCallback
errorCallback err description = hPutStrLn stderr msg 
  where msg = description ++ "\n\tCode: " ++ (show err)

-- Called when user presses buttons and stuff
--
-- The type of KeyCallBack is:
-- Window -> Key -> Int -> KeyState -> ModifierKeys -> IO () 
--
-- It sends along with it the window sending the event
-- The Int value is the scan code
--
-- This function takes an aditional `fire` paramenter which is a function
-- a -> IO (). Items passed to this function are sent to the banana event
-- handler
bananaKeyCallback :: ((GLFW.Key, GLFW.KeyState) -> IO ()) 
                     -> GLFW.KeyCallback
bananaKeyCallback fire _ key _ st _ = do
    fire (key, st)

-- Return the set of all pressed down keys from the SDL event queue
--getEvents :: IO (S.Set SDL.Keysym)
--getEvents = getEvents' S.empty 
--
--getEvents' :: S.Set SDL.Keysym -> IO (S.Set SDL.Keysym)
--getEvents' s = do
--    ev <- pollEvent
--    --case Main.pollEvent of
--    case ev of 
--      -- No more events to process
--      Nothing -> return s
--      -- Handle different event types here
--      Just event -> 
--        case eventPayload event of
--          KeyboardEvent kbd -> case getKeySymIfDown kbd of
--                                Just ks -> getEvents' (S.insert ks s)
--                                Nothing -> getEvents' s
--          _ -> getEvents' s 
--
--getKeySymIfDown :: SDL.KeyboardEventData -> Maybe SDL.Keysym
--getKeySymIfDown kbd = case keyboardEventKeyMotion kbd of
--                        Pressed -> Just $ keyboardEventKeysym kbd
--                        Released -> Nothing


--pollEvent :: IO (Maybe Event)
--pollEvent = SDL.pollEvent
--pollEvent = alloca go
--  where
--    go ptr = do result <- SDL.pollEvent ptr
--                case result of
--                  Nothing -> return Nothing
--                  _ -> fmap Just (peek ptr)

main :: IO ()
main = do
    debugMsg "[DEBUG] starting"
    -- Initialize gloss
    s <- Gloss.initState
    -- Setup error callback handler
    GLFW.setErrorCallback (Just errorCallback)
    -- GLFW needs to be initialized before it works good
    initSucc <- GLFW.init
    when (not initSucc) $ die "Error initializing GLFW"
    debugMsg "[DEBUG] Initialized GLFW"

    -- Create a window to hold something beautiful. This is returned in the
    -- Maybe monad
    mayW <- GLFW.createWindow 640 480 "Dookie Holder" Nothing Nothing
    when (isNothing mayW) $ dieAndTerm "Error creating window"
    -- Guarnateed not to be Nothing due to the previous check and crash
    let win = fromJust mayW

    -- Create an OpenGL context. This context remains usable until another
    -- is created or until the window is destroyed
    con <- GLFW.makeContextCurrent mayW

    -- Setup a banana handler for key events
    -- newAddHandler returns: (AddHandler a, a -> IO ())
    --
    -- The AddHandler can be processed in the NetworkDescription monad to
    -- create an event stream. Items can be passed to fireKey in the IO
    -- monad to create events
    (addKeyEvent, fireKey) <- newAddHandler
    debugMsg "[DEBUG] Added banana key handler"

    -- Setup a key callback. The key callback will create a reactive-banana
    -- event with incoming key strokes
    GLFW.setKeyCallback win (Just $ bananaKeyCallback fireKey)

    -- Create the banana network 
    net <- compile $ makeNetworkDescription addKeyEvent win

    -- Start the banana network
    actuate net

    randG <- getStdGen
    --let r = evalRand (myFunc 1 2 3) g :: Double
    let gd = evalRand (dfsMaze 10 10) randG
    (width, height) <- GLFW.getFramebufferSize win
    let pic = gridDescToPic (width, height) gd
    putStrLn $ show pic

    -- Enter the GLFW loop
    mainLoop s pic win

    -- Destroy the window since it is no longer being used
    GLFW.destroyWindow win
    -- Terminate GLFW before exiting like a good person
    GLFW.terminate
    debugMsg "[DEBUG] terminated GLFW, exiting"
    exitSuccess

-- Convert the passed GridDesc to a picture with the passed width and
-- height.
--
-- Note: (0,0) is in the center of the screen
--       (width, height) is the top right corner
--       (-width, -height) is the bottom left corner
gridDescToPic :: (Int, Int) -> GridDesc -> Gloss.Picture
gridDescToPic (w, h) gd = trans $ M.foldrWithKey 
                              (picAndTrans (w, h) (blockWidth, blockHeight)) 
                              Gloss.blank 
                              (blocks gd)
  where -- The width of each block is the width of the screen divided by the
        -- number of blocks
        blockWidth :: Int
        --blockWidth = (fromIntegral w) / (fromIntegral . gridWidth $ gd)
        blockWidth = w `quot` (gridWidth gd)
        blockHeight :: Int
        --blockHeight = (fromIntegral h) / (fromIntegral . gridHeight $ gd)
        blockHeight = h `quot` (gridHeight gd)
        -- All the previous drawing is done with the bottom left corner as
        -- (0,0). Gloss uses the origin as the center of the screen.
        trans = Gloss.Translate (negate (fromIntegral w) / 2)
                                (negate (fromIntegral h) / 2)

-- Given a point in GridDesc and its associated open direction, updated
-- the passed picture to hold the drawing of the block
picAndTrans :: (Int, Int) -- Width and height of picture
               -> (Int, Int) -- Width and height of individual block
               -> (Int, Int) -- Block position from GridDesc
               -> OpenDir 
               -> Gloss.Picture 
               -> Gloss.Picture
picAndTrans (w, h) (bw, bh) (x, y) dir pic = pic <> posBl
  where 
        bl = blockToPic (bw, bh) (openDirToBlock dir)
        --bl = Gloss.Circle 10.0
        -- Need to shift the block from the origin to its proper position
        posBl = Gloss.translate 
                    (fromIntegral $ bw * x) 
                    (fromIntegral $ bh * y) 
                    --((fromIntegral x) / (fromIntegral w))
                    --((fromIntegral y) / (fromIntegral h))
                    bl

-- Create a picture from a block. This draws the block with (0,0) as the lower
-- left corner. The passed Ints are the width and height of the block
blockToPic :: (Int, Int) -> Block -> Gloss.Picture
blockToPic (w, h) b = northWall <> southWall <> eastWall <> westWall
  where northWall = if northOpen b 
                      then Gloss.blank 
                      else Gloss.color 
                           Gloss.black 
                           (Gloss.line [ (0, hf), (wf,  hf) ])
        southWall = if southOpen b 
                      then Gloss.blank
                      else Gloss.color 
                           Gloss.black 
                           (Gloss.line [ (0, 0), (wf,  0) ])
        eastWall = if eastOpen b 
                      then Gloss.blank
                      else Gloss.color 
                           Gloss.black 
                           (Gloss.line [ (wf, 0), (wf,  hf) ])
        westWall = if westOpen b 
                      then Gloss.blank
                      else Gloss.color 
                           Gloss.black 
                           (Gloss.line [ (0, 0), (0,  hf) ])
        wf = fromIntegral w
        hf = fromIntegral h

mainLoop :: Gloss.State -> Gloss.Picture -> GLFW.Window -> IO ()
mainLoop gs pic win = do
    -- The viewport needs the framebuffer size
    (width, height) <- GLFW.getFramebufferSize win
    -- Set the viewport to be the entire size of the window
    viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
    -- Change to orthogonal projection
    clear [ColorBuffer]
    matrixMode $= Projection
    loadIdentity
    let ratio = fromIntegral width / fromIntegral height
    ortho (negate ratio) ratio (negate 1.0) 1.0 1.0 (negate 1.0)
    matrixMode $= Modelview 0
    loadIdentity

    -- Use vsync
    GLFW.swapInterval 1
    --Gloss.displayPicture (width, height) Gloss.white gs 1.0 pic2
    Gloss.displayPicture (width, height) Gloss.white gs 1.0 pic

    -- Draw some stuff
    --renderPrimitive Lines $ do
    --  vertex (Vertex3 (1.0 :: GLfloat) 1.0 0)
    --  vertex (Vertex3 (0.0 :: GLfloat) 0.0 0)  


    -- GLFW has a front and back buffer. Since we have finished our
    -- rendering for this loop, swap the current buffer to the front and
    -- continue again
    GLFW.swapBuffers win
    GLFW.pollEvents
    close <- (GLFW.windowShouldClose win) 
    unless close $ mainLoop gs pic win

-- The first argument is an add handler attached to GLFW key press events
-- The second argument is the window being managed
makeNetworkDescription :: Frameworks t
                       => AddHandler (GLFW.Key, GLFW.KeyState)
                       -> GLFW.Window
                       -> Moment t ()
makeNetworkDescription keyH win = do
    eKeys <- fromAddHandler keyH
    let 
        ek = eKeys
        eEsc = pressedESCKey <$> ek
    reactimate $ fmap print ek
    reactimate $ fmap (GLFW.setWindowShouldClose win) eEsc
  where 
        pressedESCKey :: (GLFW.Key, GLFW.KeyState) -> Bool
        pressedESCKey (k, s) = k == GLFW.Key'Escape
                               && (s == GLFW.KeyState'Pressed
                                  || s == GLFW.KeyState'Repeating)
