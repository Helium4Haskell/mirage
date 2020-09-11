{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  )
where

import           Data.GI.Base

import           Graphics.Rendering.Cairo
import           Graphics.Rendering.Cairo.Internal
                                                ( Render(runRender) )
import           Graphics.Rendering.Cairo.Types ( Cairo(Cairo) )
import           Foreign.Ptr                    ( castPtr )
import           Control.Monad.Reader           ( ReaderT(runReaderT) )

import qualified GI.Cairo                      as Cairo
import qualified GI.Gdk                        as Gdk
import qualified GI.Gtk                        as Gtk
import qualified GI.Gio                        as Gio

import           Mirage

-- * Mouse and button press signals to respond to input from the user. (Use
--   widgetAddEvents to enable events you wish to receive.)
-- * The realize signal to take any necessary actions when the widget is
--   instantiated on a particular display. (Create GDK resources in response
--   to this signal.)
-- * The sizeAllocate signal to take any necessary actions when the widget
--   changes size.
-- * The draw signal to handle redrawing the contents of the widget.

buttonPressEvent :: Gtk.DrawingArea -> Gdk.EventButton -> IO Bool
buttonPressEvent area eventButton = do

  -- putStrLn "BUTTON PRESS EVENT"

  -- axes      <- get eventButton #axes
  -- button    <- get eventButton #button
  -- -- device    <- get eventButton #device
  -- sendEvent <- get eventButton #sendEvent
  -- state     <- get eventButton #state
  -- time      <- get eventButton #time
  -- type'     <- get eventButton #type
  -- -- window    <- get eventButton #window
  -- x         <- get eventButton #x
  -- xRoot     <- get eventButton #xRoot
  -- y         <- get eventButton #y
  -- yRoot     <- get eventButton #yRoot

  -- print ("axes", axes)
  -- print ("button", button)
  -- -- print ("device", device)
  -- print ("sendEvent", sendEvent)
  -- print ("state", state)
  -- print ("time", time)
  -- print ("type", type')
  -- -- print ("window", window)
  -- print ("x", x)
  -- print ("xRoot", xRoot)
  -- print ("y", y)
  -- print ("yRoot", yRoot)

  -- putStrLn ""

  return True

buttonReleaseEvent :: Gtk.DrawingArea -> Gdk.EventButton -> IO Bool
buttonReleaseEvent area eventButton = do

  -- putStrLn "BUTTON RELEASE EVENT"

  -- axes      <- get eventButton #axes
  -- button    <- get eventButton #button
  -- -- device    <- get eventButton #device
  -- sendEvent <- get eventButton #sendEvent
  -- state     <- get eventButton #state
  -- time      <- get eventButton #time
  -- type'     <- get eventButton #type
  -- -- window    <- get eventButton #window
  -- x         <- get eventButton #x
  -- xRoot     <- get eventButton #xRoot
  -- y         <- get eventButton #y
  -- yRoot     <- get eventButton #yRoot

  -- print ("axes", axes)
  -- print ("button", button)
  -- -- print ("device", device)
  -- print ("sendEvent", sendEvent)
  -- print ("state", state)
  -- print ("time", time)
  -- print ("type", type')
  -- -- print ("window", window)
  -- print ("x", x)
  -- print ("xRoot", xRoot)
  -- print ("y", y)
  -- print ("yRoot", yRoot)

  -- putStrLn ""

  return True

motionNotifyEvent :: Gtk.DrawingArea -> Gdk.EventMotion -> IO Bool
motionNotifyEvent area eventMotion = do

  -- putStrLn "BUTTON RELEASE EVENT"

  -- axes      <- get eventMotion #axes
  -- isHint    <- get eventMotion #isHint
  -- sendEvent <- get eventMotion #sendEvent
  -- state     <- get eventMotion #state
  -- time      <- get eventMotion #time
  -- type'     <- get eventMotion #type
  -- x         <- get eventMotion #x
  -- xRoot     <- get eventMotion #xRoot
  -- y         <- get eventMotion #y
  -- yRoot     <- get eventMotion #yRoot

  -- print ("axes", axes)
  -- print ("isHint", isHint)
  -- print ("sendEvent", sendEvent)
  -- print ("state", state)
  -- print ("time", time)
  -- print ("type", type')
  -- print ("x", x)
  -- print ("xRoot", xRoot)
  -- print ("y", y)
  -- print ("yRoot", yRoot)

  -- putStrLn ""

  return True

realize :: Gtk.DrawingArea -> IO ()
realize area = do


  return ()

sizeAllocate :: Gtk.DrawingArea -> Gdk.Rectangle -> IO ()
sizeAllocate area rectangle = do
  return ()

draw :: Gtk.DrawingArea -> Cairo.Context -> IO Bool
draw area ctx = do
  print =<< #getEvents area


  r               <- #getAllocation area
  x               <- fromIntegral <$> get r #x
  y               <- fromIntegral <$> get r #y
  width           <- fromIntegral <$> get r #width
  height          <- fromIntegral <$> get r #height

  refStyleContext <- #getStyleContext area

  Gtk.renderBackground refStyleContext ctx x y width height

  Gdk.cairoSetSourceRgba ctx
    =<< #getColor refStyleContext
    =<< #getState refStyleContext

  renderWithContext ctx (renderRule isOrderedExample)

  return True

-- | This function bridges gi-cairo with the hand-written cairo
-- package. It takes a `GI.Cairo.Context` (as it appears in gi-cairo),
-- and a `Render` action (as in the cairo lib), and renders the
-- `Render` action into the given context.
renderWithContext :: Cairo.Context -> Render a -> IO a
renderWithContext ct r =
  withManagedPtr ct $ \p -> runReaderT (runRender r) (Cairo (castPtr p))

activateApp :: Gtk.Application -> IO ()
activateApp app = do
  window <- Gtk.applicationWindowNew app
  area   <- new Gtk.DrawingArea []

  #add window area

  on area #realize            (realize area)
  on area #sizeAllocate       (sizeAllocate area)
  on area #draw               (draw area)

  on area #buttonPressEvent   (buttonPressEvent area)
  on area #buttonReleaseEvent (buttonReleaseEvent area)
  on area #motionNotifyEvent  (motionNotifyEvent area)

  #addEvents
    area
    [ Gdk.EventMaskButtonPressMask
    , Gdk.EventMaskButtonReleaseMask
    , Gdk.EventMaskPointerMotionMask
    ]

  #showAll window

  return ()

main :: IO ()
main = do
  app <- new
    Gtk.Application
    [ #applicationId := "uu.ag.mirage"
    , #flags := [Gio.ApplicationFlagsFlagsNone]
    ]

  on app #activate (activateApp app)
  Gio.applicationRun app Nothing

  return ()
