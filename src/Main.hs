module Main where
import qualified Graphics.Gloss as Gloss
import qualified Graphics.Gloss.Rendering as Gloss
--import Graphics.Rendering.OpenGL.Raw -- as gl*
--import System.Mem (performGC)
import Control.Monad (when
                     , unless)
import Control.Monad.Random (evalRand
                            , getStdGen)
--import Data.Monoid ((<>))
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
--import qualified Data.Map as M
-- import Data.Text (pack)
--import Control.Applicative ((<$>))
--import Reactive.Banana.Frameworks (newAddHandler
--                                  , actuate
--                                  , compile
--                                  , fromAddHandler
--                                  , Frameworks
--                                  , reactimate
--                                  , reactimate'
--                                  , changes
--                                  , AddHandler
--                                  )
--import Reactive.Banana.Switch (Moment)
--import Reactive.Banana.Combinators (Behavior
--                                   , stepper
--                                   , Event (..)
--                                   , accumB)
import qualified Data.Set as S

import Level (gridWidth
             , gridHeight
             , gridDescToPic
             , dfsMaze
             )

import World (World
             , fromGridDesc
             , fromGridDescWithPlPos
             , playerPos
             , worldToPicture
             , worldTilesToPicture
             , updatePlayerPos
             , inWall
             )

playerXVel :: Double
playerXVel = 1.5
playerYVel :: Double
playerYVel = 1.5

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
--bananaKeyCallback :: ((GLFW.Key, GLFW.KeyState) -> IO ()) 
--                     -> GLFW.KeyCallback
--bananaKeyCallback fire _ key _ st _ = do
--    fire (key, st)

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
    --(addKeyEvent, fireKey) <- newAddHandler
    --debugMsg "[DEBUG] Added banana key handler"
    -- Similarly, create an event handler to communicate the amount of time
    -- passed 
    --(timeEvent, fireTime) <- newAddHandler

    -- Setup a key callback. The key callback will create a reactive-banana
    -- event with incoming key strokes
    --GLFW.setKeyCallback win (Just $ bananaKeyCallback fireKey)

    -- Create the banana network 
    --net <- compile $ makeNetworkDescription addKeyEvent timeEvent win

    -- Start the banana network
    --actuate net

    randG <- getStdGen
    --let r = evalRand (myFunc 1 2 3) g :: Double
    let gd = evalRand (dfsMaze 10 10) randG
    (width, height) <- GLFW.getFramebufferSize win
    --let pic = gridDescToPic (width, height) gd
    --let pic = worldToPicture (width, height) 
    --                         (gridWidth gd, gridHeight gd) 
    --                         (fromGridDescWithPlPos gd (5, 5))
    -- Enter the GLFW loop
    --mainLoop fireTime s pic win
    mainLoop s win (fromGridDescWithPlPos gd (0.5, 0.5))

    -- Destroy the window since it is no longer being used
    GLFW.destroyWindow win
    -- Terminate GLFW before exiting like a good person
    GLFW.terminate
    debugMsg "[DEBUG] terminated GLFW, exiting"
    exitSuccess

-- Query the current state of all the relevant keys. Keys will only be included
-- in the resulting set if they are pressed or repeating
getRelevantKeys :: GLFW.Window -> IO (S.Set (GLFW.Key))
getRelevantKeys win = do
    keyState <- mapM (GLFW.getKey win) keys
    let ks = filter (isPressed) $ zip keys keyState
    return $ S.fromList (map fst ks)
  where keys = [GLFW.Key'W
               , GLFW.Key'A
               , GLFW.Key'S
               , GLFW.Key'D
               , GLFW.Key'Escape
               ]
        isPressed :: (GLFW.Key, GLFW.KeyState) -> Bool
        isPressed (_, s) = if s == GLFW.KeyState'Pressed 
                              || s == GLFW.KeyState'Repeating
                                then True
                                else False

-- If the exit button is in the passed set of keys then fire an exit event
setCloseIfESC :: GLFW.Window -> S.Set (GLFW.Key) -> IO ()
setCloseIfESC win s = do
    GLFW.setWindowShouldClose win (S.member GLFW.Key'Escape s) 

-- Given the set of currently pressed keys return the current velocity in the
-- X direction
getXVel :: S.Set (GLFW.Key) -> Double
getXVel s = if (S.member GLFW.Key'A s)
               && (not $ S.member GLFW.Key'D s)
               then negate playerXVel
               else if (S.member GLFW.Key'D s)
                      && (not $ S.member GLFW.Key'A s)
                then playerXVel
                else 0

-- Given the set of currently pressed keys return the current velocity in the
-- Y direction
getYVel :: S.Set (GLFW.Key) -> Double
getYVel s = if (S.member GLFW.Key'S s)
               && (not $ S.member GLFW.Key'W s)
               then (negate playerYVel)
               else if (S.member GLFW.Key'W s)
                      && (not $ S.member GLFW.Key'S s)
                then playerYVel
                else 0


--mainLoop :: (Double -> IO ()) -- Event stream generation for time
mainLoop :: Gloss.State 
            -> GLFW.Window 
            -> World
            -> IO ()
--mainLoop fireTime gs pic win = do
mainLoop gs win world = do
    keys <- getRelevantKeys win
    setCloseIfESC win keys
    -- Get the time since we last were here. Set the time to zero to record the
    -- total time the main-loop takes to execute
    mayTimeD <- GLFW.getTime
    GLFW.setTime 0 
    -- Not too sure but it seems the time returns nothing when the time is
    -- zero. This would mean no time has passed yet so re-run the loop to waste
    -- some time
    --when (isNothing mayTimeD) $ mainLoop fireTime gs pic win
    let timeD = fromJust mayTimeD
    -- Update the world based on the current stuff
    let (posX, posY) = playerPos world
    let xVel = getXVel keys
    let yVel = getYVel keys
    let newPos = (posX + timeD * xVel, posY + timeD * yVel)
    let walRes = inWall world newPos
    let upStr = case walRes of 
                  (Nothing, Nothing) -> ""
                  _ -> show walRes
    when (not $ null upStr) $ putStrLn upStr
    --let world2 = world { playerPos = newPos }
    let world2 = updatePlayerPos world newPos

    --fireTime timeD
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
    --let curFrame = worldToPicture (width, height) (3, 3) world2
    let curFrame = worldTilesToPicture (width, height) (3, 3) world2
    Gloss.displayPicture (width, height) Gloss.white gs 1.0 curFrame

    -- GLFW has a front and back buffer. Since we have finished our
    -- rendering for this loop, swap the current buffer to the front and
    -- continue again
    GLFW.swapBuffers win
    GLFW.pollEvents
    close <- (GLFW.windowShouldClose win) 
    unless close $ mainLoop gs win world2


-- The first argument is an add handler attached to GLFW key press events
-- The second argument is the window being managed
-- makeNetworkDescription :: Frameworks t => 
--                        AddHandler (GLFW.Key, GLFW.KeyState)
--                        -> AddHandler (Double)
--                        -> GLFW.Window
--                        -> Moment t ()
-- makeNetworkDescription keyH timeH win = do
--     eKeys <- fromAddHandler keyH
--     eTDelta <- fromAddHandler timeH
--     let 
--         ek = eKeys
--         --eXPlayerPos = xVelFromKey <$> ek
--         --eYPlayerPos = yVelFromKey <$> ek
--         eEsc = pressedESCKey <$> ek
--         bTime = stepper 0.0 eTDelta
--         bMoveLeft = stepper False $ pressedKey GLFW.Key'A <$> ek
--         bMoveRight = stepper False $ pressedKey GLFW.Key'D <$> ek
--         bMoveUp = stepper False $ pressedKey GLFW.Key'W <$> ek
--         bMoveDown = stepper False $ pressedKey GLFW.Key'S <$> ek
--         bXVel = xVelFromKeys <$> bMoveLeft <*> bMoveRight
--         bYVel = yVelFromKeys <$> bMoveUp <*> bMoveDown
--         bXPosDelta = (*) <$> bTime <*> bXVel 
--         -- X position behavior. Updated by the network. Initialized to zero
--         --bXPlayerPos :: Behavior t Double
--         --bXPlayerPos = accumB 0 eXPlayerPos
--         ---- Y position behavior. Updated by the network
--         --bYPlayerPos :: Behavior t Double
--         --bYPlayerPos = accumB 0 eYPlayerPos
--     eXVelChanged <- changes bXVel
--     eYVelChanged <- changes bYVel
--     eXPosDelta <- changes bXPosDelta
--     reactimate $ fmap print ek
--     reactimate' $ fmap (\n -> putStrLn ("XVel: " ++ show n))
--                <$> eXVelChanged
--     reactimate' $ fmap (\n -> putStrLn ("YVel: " ++ show n))
--                <$> eYVelChanged
--     reactimate' $ fmap (\n -> putStrLn ("Delta X: " ++ show n))
--                <$> eXPosDelta
--     reactimate $ fmap (GLFW.setWindowShouldClose win) eEsc
--   where 
--         -- Given the left and right key state return the X velocity
--         xVelFromKeys :: Bool -> Bool -> Double
--         xVelFromKeys l r = if l && (not r) then -0.1
--                                 else if (not l) && r then 0.1
--                                 else 0
--         yVelFromKeys :: Bool -> Bool -> Double
--         yVelFromKeys u d = if u && (not d) then 0.1
--                                 else if (not u) && d then -0.1
--                                 else 0
--         pressedKey :: GLFW.Key -> (GLFW.Key, GLFW.KeyState) -> Bool
--         pressedKey key (k, s) = k == key
--                                && (s == GLFW.KeyState'Pressed
--                                    || s == GLFW.KeyState'Repeating)
--         pressedESCKey :: (GLFW.Key, GLFW.KeyState) -> Bool
--         pressedESCKey = pressedKey (GLFW.Key'Escape)
--         xPosDeltaEv :: Event t Double -- Change in time
--                        -> Event t (GLFW.Key, GLFW.KeyState) -- Pressed key
--                        -> Event t (Double -> Double)
--         xPosDeltaEv time ks = fmap (+) delta
--           where vel = fmap (\b -> if True then 0.1 else 0)
--                            (pressedKey GLFW.Key'D <$> ks)
--                 delta =  (*) <$> vel <*> time 
