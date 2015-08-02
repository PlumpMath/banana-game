module Main where
--import Control.Monad
--import qualified Graphics.Gloss as Gloss
--import qualified Graphics.Gloss.Rendering as Gloss
--import Graphics.Rendering.OpenGL.Raw -- as gl*
--import System.Mem (performGC)
import Control.Monad (when
                     , unless)
import qualified Graphics.UI.GLFW as GLFW
import System.Exit (exitFailure
                   , exitSuccess)
import System.IO (hPutStrLn
                 , stderr)
import Data.Maybe (fromJust, isNothing)
import Control.Applicative ((<$>))
import Reactive.Banana.Frameworks (newAddHandler
                                  , actuate
                                  , compile
                                  , fromAddHandler
                                  , Frameworks
                                  , reactimate
                                  , AddHandler)
import Reactive.Banana.Switch (Moment)

import Level (GridDesc)

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

main :: IO ()
main = do
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

    -- Enter the GLFW loop
    mainLoop win

    -- Destroy the window since it is no longer being used
    GLFW.destroyWindow win
    -- Terminate GLFW before exiting like a good person
    GLFW.terminate
    debugMsg "[DEBUG] terminated GLFW, exiting"
    exitSuccess

mainLoop :: GLFW.Window -> IO ()
mainLoop win = do
    -- GLFW has a front and back buffer. Since we have finished our
    -- rendering for this loop, swap the current buffer to the front and
    -- continue again
    GLFW.swapBuffers win
    GLFW.pollEvents
    close <- (GLFW.windowShouldClose win) 
    unless close $ mainLoop win

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
