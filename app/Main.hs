{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Main
  ( main
  )
where

import           Data.GI.Base
import           Data.GI.Base.GType             ( gtypeString )

import           Graphics.Rendering.Cairo.Internal
                                                ( Render(runRender) )
import           Graphics.Rendering.Cairo.Types ( Cairo(Cairo) )
import           Foreign.Ptr                    ( castPtr )
import           Control.Monad.Reader           ( ReaderT(runReaderT) )

import           GI.Cairo                       ( Context(..) )
import qualified GI.Gdk                        as Gdk
import qualified GI.Gtk                        as Gtk
import qualified GI.Gio                        as Gio

import           Data.Aeson                     ( eitherDecode )
import qualified Data.ByteString.Lazy          as ByteString

import           Data.Text                      ( Text )

import           Data.IORef
import           Data.Foldable                  ( for_ )

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
buttonPressEvent _area _eventButton = do

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
buttonReleaseEvent _area _eventButton = do

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
motionNotifyEvent _area _eventMotion = do

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
realize _area = do


  return ()

sizeAllocate :: Gtk.DrawingArea -> Gdk.Rectangle -> IO ()
sizeAllocate _area _rectangle = do
  return ()

draw
  :: IORef (Maybe (Text, Text))
  -> IORef (Maybe Grammar)
  -> Gtk.DrawingArea
  -> Context
  -> IO Bool
draw selectedRef mayGrammarRef area ctx = do
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

  mayGrammar  <- readIORef mayGrammarRef
  maySelected <- readIORef selectedRef
  case (,) <$> mayGrammar <*> maySelected of
    Nothing -> return ()
    Just (grammar, (nont, prod)) ->
      renderWithContext ctx (renderGrammar (width, height) grammar nont prod)

  return True

-- | This function bridges gi-cairo with the hand-written cairo
-- package. It takes a `GI.Cairo.Context` (as it appears in gi-cairo),
-- and a `Render` action (as in the cairo lib), and renders the
-- `Render` action into the given context.
renderWithContext :: Context -> Render a -> IO a
renderWithContext ct r =
  withManagedPtr ct $ \p -> runReaderT (runRender r) (Cairo (castPtr p))

openFile :: Gtk.TreeStore -> IORef (Maybe Grammar) -> IO ()
openFile store mayGrammarRef = do
  dialog <- new Gtk.FileChooserDialog
                [#title := "Open File", #action := Gtk.FileChooserActionOpen]

  _ <- #addButton dialog
                  "Cancel"
                  (fromIntegral (fromEnum Gtk.ResponseTypeCancel))
  _ <- #addButton dialog "Open" (fromIntegral (fromEnum Gtk.ResponseTypeAccept))

  fileFilter <- new Gtk.FileFilter []
  #addPattern fileFilter "*.mirage"
  _   <- #setFilter dialog fileFilter

  res <- #run dialog

  case toEnum (fromIntegral res) of
    Gtk.ResponseTypeAccept -> do
      mayFileName <- #getFilename dialog
      case mayFileName of
        Nothing       -> return ()
        Just fileName -> do
          fileContents <- ByteString.readFile fileName
          mayGrammar   <- either ((Nothing <$) . print)
                                 (return . Just)
                                 (eitherDecode fileContents)
          case mayGrammar of
            Nothing      -> return ()
            Just grammar -> do
              writeIORef mayGrammarRef (Just grammar)

              #clear store
              for_ (nontsAndProds grammar) $ \(nont, prods) -> do
                nontIter <- #append store Nothing
                #setValue store nontIter 0 =<< toGValue (Just nont)
                for_ prods $ \prod -> do
                  prodIter <- #append store (Just nontIter)
                  #setValue store prodIter 0 =<< toGValue (Just prod)
                  -- prodValue <- #getValue store prodIter 0
                  -- mayProd <- fromGValue prodValue
                  -- case mayProd of
                  --   Nothing -> return ()
                  --   Just prod -> Text.putStrLn prod
    _ -> return ()

  #destroy dialog

-- https://developer.gnome.org/gtk3/stable/TreeWidget.html
setupTree
  :: Gtk.DrawingArea
  -> IO (Gtk.TreeStore, Gtk.TreeView, IORef (Maybe (Text, Text)))
setupTree area = do
  selectedRef <- newIORef Nothing

  store       <- Gtk.treeStoreNew [gtypeString, gtypeString]

  tree        <- Gtk.treeViewNewWithModel store
  #setHeadersVisible tree False

  renderer <- new Gtk.CellRendererText []
  column   <- new Gtk.TreeViewColumn []
  #packStart column renderer False
  #addAttribute column renderer "text" 0
  _      <- #appendColumn tree column

  select <- #getSelection tree
  #setMode select Gtk.SelectionModeSingle
  _ <- on select #changed $ do
    (isSelected, model, iter) <- #getSelected select
    if isSelected
      then do
        prodValue             <- #getValue model iter 0
        (hasParent, nontIter) <- #iterParent model iter
        if hasParent
          then do
            nontValue <- #getValue model nontIter 0

            mayProd   <- fromGValue prodValue
            mayNont   <- fromGValue nontValue
            case (,) <$> mayProd <*> mayNont of
              Nothing           -> return () -- "Empty selection"
              Just (prod, nont) -> do
                writeIORef selectedRef (Just (nont, prod))
                #queueDraw area
          else return () -- No parent
      else return () -- No selection

  return (store, tree, selectedRef)

activateApp :: Gtk.Application -> IO ()
activateApp app = do
  window         <- Gtk.applicationWindowNew app

  vBox           <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
  toolbar        <- new Gtk.Toolbar []
  openFileButton <- new Gtk.ToolButton [#label := "Open File"]
  hBox           <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal]
  area           <- new Gtk.DrawingArea []
  scroll <- new Gtk.ScrolledWindow [#hscrollbarPolicy := Gtk.PolicyTypeNever]

  mayGrammarRef  <- newIORef Nothing

  #insert toolbar openFileButton 0
  #packStart vBox toolbar False False 0


  (store, tree, selectedRef) <- setupTree area

  #add scroll tree
  #packStart hBox scroll False False 0

  _ <- on openFileButton #clicked (openFile store mayGrammarRef)

  #packStart hBox area True True 0

  #packStart vBox hBox True True 0
  #add window vBox

  _ <- on area #realize (realize area)
  _ <- on area #sizeAllocate (sizeAllocate area)
  _ <- on area #draw (draw selectedRef mayGrammarRef area)

  _ <- on area #buttonPressEvent (buttonPressEvent area)
  _ <- on area #buttonReleaseEvent (buttonReleaseEvent area)
  _ <- on area #motionNotifyEvent (motionNotifyEvent area)

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

  _ <- on app #activate (activateApp app)
  _ <- Gio.applicationRun app Nothing

  return ()
