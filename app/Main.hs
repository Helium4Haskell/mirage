{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
module Main
  ( main
  ) where

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
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text

import           Data.IORef
import           Data.Foldable                  ( for_
                                                , traverse_
                                                , fold
                                                )
import           Control.Monad                  ( when
                                                , (>=>)
                                                )
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Set                      as Set
import           Data.Traversable               ( for )

import           Mirage

data State = State
  { stateGrammar      :: !(Maybe Grammar)
  , stateSelected     :: !(Maybe (Text, Text))
  , stateHideImplicit :: !Bool
  , stateBBTree       :: !(Maybe (BBTree AttrInfo))
  }

data Reader = Reader
  { readerFilterWindow  :: !Gtk.Window
  , readerCodeWindow    :: !Gtk.Window
  , readerCodeBuffer    :: !Gtk.TextBuffer
  , readerDisabledStore :: !Gtk.ListStore
  , readerEnabledStore  :: !Gtk.ListStore
  , readerSidebarStore  :: !Gtk.TreeStore
  , readerArea          :: !Gtk.DrawingArea
  , readerState         :: !(IORef State)
  }

buttonPressEvent :: Reader -> Gdk.EventButton -> IO Bool
buttonPressEvent (Reader {..}) eventButton = do
  x <- get eventButton #x
  y <- get eventButton #y

  s <- readIORef readerState
  for_ (stateBBTree s) $ \bbForest -> case lookupBBTree (x, y) bbForest of
    Just x  -> populateCode readerCodeWindow readerCodeBuffer x
    Nothing -> pure ()

  return True

populateCode :: Gtk.Window -> Gtk.TextBuffer -> AttrInfo -> IO ()
populateCode window buffer (TargetInfo tp code origin) = do
  set buffer [#text := Text.strip code]
  #showAll window
populateCode _ _ (SourceInfo _) = return ()

buttonReleaseEvent :: Reader -> Gdk.EventButton -> IO Bool
buttonReleaseEvent (Reader {..}) _eventButton = pure True

motionNotifyEvent :: Reader -> Gdk.EventMotion -> IO Bool
motionNotifyEvent _reader _eventMotion = do
  -- x         <- get eventMotion #x
  -- y         <- get eventMotion #y

  return True

realize :: Reader -> IO ()
realize _reader = do
  return ()

sizeAllocate :: Reader -> Gdk.Rectangle -> IO ()
sizeAllocate _reader _rectangle = do
  return ()

draw :: Reader -> Context -> IO Bool
draw (Reader {..}) ctx = do
  r               <- #getAllocation readerArea
  x               <- fromIntegral <$> get r #x
  y               <- fromIntegral <$> get r #y
  width           <- fromIntegral <$> get r #width
  height          <- fromIntegral <$> get r #height

  refStyleContext <- #getStyleContext readerArea

  Gtk.renderBackground refStyleContext ctx x y width height

  Gdk.cairoSetSourceRgba ctx
    =<< #getColor refStyleContext
    =<< #getState refStyleContext

  -- collect all enabled attributes
  (hasNext, iter) <- #getIterFirst readerEnabledStore
  let loop s = do
        s' <-
          ($ s)
          .   maybe id Set.insert
          <$> (fromGValue @(Maybe Text) =<< #getValue readerEnabledStore iter 0)
        hasNext <- #iterNext readerEnabledStore iter
        if hasNext then loop s' else return s'
  enabled    <- if hasNext then loop mempty else return mempty

  State {..} <- readIORef readerState
  for ((,) <$> stateGrammar <*> stateSelected) $ \(grammar, (nont, prod)) -> do
    bbForest <- renderWithContext
      ctx
      (renderGrammar (width, height) grammar nont prod stateHideImplicit enabled
      )
    let (w, h) = bbSize bbForest
    #setSizeRequest readerArea (ceiling w) (ceiling h)
    atomicModifyIORef' readerState
      $ \s -> (s { stateBBTree = Just bbForest }, ())

  return True

-- | This function bridges gi-cairo with the hand-written cairo
-- package. It takes a `GI.Cairo.Context` (as it appears in gi-cairo),
-- and a `Render` action (as in the cairo lib), and renders the
-- `Render` action into the given context.
renderWithContext :: Context -> Render a -> IO a
renderWithContext ct r =
  withManagedPtr ct $ \p -> runReaderT (runRender r) (Cairo (castPtr p))

openFile :: Reader -> IO ()
openFile (Reader {..}) = do
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
    Gtk.ResponseTypeAccept -> #getFilename dialog >>= traverse_ \fileName -> do
      fileContents <- ByteString.readFile fileName
      mayGrammar   <- either ((Nothing <$) . print)
                             (return . Just)
                             (eitherDecode fileContents)
      for_ mayGrammar $ \grammar -> do
        atomicModifyIORef' readerState $ \s ->
          (s { stateGrammar = Just grammar, stateSelected = Nothing }, ())

        let StaticInfo nontsAndProds attrNames = staticInfo grammar

        #clear readerSidebarStore
        for_ nontsAndProds $ \(nont, prods) -> do
          nontIter <- #append readerSidebarStore Nothing
          #setValue readerSidebarStore nontIter 0 =<< toGValue (Just nont)
          for_ prods $ \prod -> do
            prodIter <- #append readerSidebarStore (Just nontIter)
            #setValue readerSidebarStore prodIter 0 =<< toGValue (Just prod)

        #clear readerDisabledStore

        #clear readerEnabledStore
        for_ attrNames $ \attr -> do
          attrIter <- #append readerEnabledStore
          #setValue readerEnabledStore attrIter 0 =<< toGValue (Just attr)

    _ -> return ()

  #destroy dialog

-- https://developer.gnome.org/gtk3/stable/TreeWidget.html
setupTree :: IORef State -> IO () -> IO (Gtk.TreeStore, Gtk.TreeView)
setupTree stateRef redraw = do

  store <- Gtk.treeStoreNew [gtypeString, gtypeString]

  tree  <- Gtk.treeViewNewWithModel store
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
    when isSelected $ do
      prodValue             <- #getValue model iter 0
      (hasParent, nontIter) <- #iterParent model iter
      when hasParent $ do
        nontValue <- #getValue model nontIter 0

        mayProd   <- fromGValue @(Maybe Text) prodValue
        mayNont   <- fromGValue @(Maybe Text) nontValue

        for_ mayProd $ \prod -> for_ mayNont $ \nont -> do
          atomicModifyIORef' stateRef
            $ \s -> (s { stateSelected = Just (nont, prod) }, ())
          redraw

  return (store, tree)

  -- Info about actions: https://wiki.gnome.org/HowDoI/GAction
setupMenubar :: Reader -> Gtk.Application -> IO () -> IO ()
setupMenubar reader@(Reader {..}) app redraw = do

  -- open file action
  openFileAction <- new Gio.SimpleAction [#name := "openFile"]
  _              <- on openFileAction #activate (\_ -> openFile reader)
  #addAction app openFileAction

  -- hide implicit action
  hideImplicitAction <- new
    Gio.SimpleAction
    [#name := "hideImplicit", #state :=> toGVariant False]
  _ <- on hideImplicitAction #changeState $ traverse_ $ \v' -> do
    #setState hideImplicitAction v'
    fromGVariant v' >>= traverse_ \v -> do
      atomicModifyIORef' readerState $ \s -> (s { stateHideImplicit = v }, ())
      redraw
  #addAction app hideImplicitAction

  -- filter window action
  filterWindowAction <- new
    Gio.SimpleAction
    [#name := "toggleFilterWindow", #state :=> toGVariant False]
  _ <-
    on filterWindowAction #changeState
    $   traverse_
    $   fromGVariant
    >=> traverse_ \v -> (if v then #showAll else #hide) readerFilterWindow
  #addAction app filterWindowAction
  on readerFilterWindow #hide
    $ set filterWindowAction [#state :=> toGVariant False]
  on readerFilterWindow #show
    $ set filterWindowAction [#state :=> toGVariant True]

  menu     <- new Gio.Menu []
  fileMenu <- new Gio.Menu []
  viewMenu <- new Gio.Menu []

  #append fileMenu (Just "Open") (Just "app.openFile")

  #freeze fileMenu
  #appendSubmenu menu (Just "File") fileMenu

  #append viewMenu (Just "Hide implicit") (Just "app.hideImplicit")
  #append viewMenu (Just "Toggle Filter Window") (Just "app.toggleFilterWindow")

  #freeze viewMenu
  #appendSubmenu menu (Just "View") viewMenu

  #freeze menu

  #setMenubar app (Just menu)

insert :: Text -> Gtk.ListStore -> IO ()
insert attrName store = do
  (canIterate, iter) <- #getIterFirst store
  let loop = do
        attrName' <-
          fmap (fromMaybe "") . fromGValue @(Maybe Text) =<< #getValue store
                                                                       iter
                                                                       0

        if (attrName' <= attrName)
          then do
            hasNext <- #iterNext store iter
            if hasNext then loop else return False
          else return True

  canInsert <- if canIterate then loop else return False

  iter <- if canInsert then #insertBefore store (Just iter) else #append store
  #setValue store iter 0 =<< toGValue (Just attrName)

  return ()

toggleAttribute :: Gtk.ListStore -> Gtk.ListStore -> Gtk.TreeIter -> IO Bool
toggleAttribute from to iter = do
  mayAttrName <- fromGValue @(Maybe Text) =<< #getValue from iter 0

  for_ mayAttrName $ \attrName -> insert attrName to
  #remove from iter

toggleAttributeTrans
  :: Graph -> Gtk.ListStore -> Gtk.ListStore -> Gtk.TreeIter -> IO ()
toggleAttributeTrans graph from to iter = do
  mayAttrName <- fromGValue @(Maybe Text) =<< #getValue from iter 0
  for_ mayAttrName $ \attrName -> do

    let closure = fold (take 2 (transitiveClosure attrName graph))

    (canIterate, iter) <- #getIterFirst from

    let loop = do
          attrName' <-
            fmap (fromMaybe "") . fromGValue @(Maybe Text) =<< #getValue from
                                                                         iter
                                                                         0
          hasNext <- if attrName' `Set.member` closure
            then do
              toggleAttribute from to iter
            else do
              #iterNext from iter
          if hasNext then loop else return ()
    if canIterate then loop else return ()

moveAll :: Gtk.ListStore -> Gtk.ListStore -> IO ()
moveAll from to = do
  (canIterate, iter) <- #getIterFirst from

  let loop = do
        hasNext <- toggleAttribute from to iter
        when hasNext loop

  when canIterate loop

setupFilterWindow
  :: IORef State -> IO () -> IO (Gtk.Window, Gtk.ListStore, Gtk.ListStore)
setupFilterWindow stateRef redraw = do
  filterWindow <- new Gtk.Window [#typeHint := Gdk.WindowTypeHintDialog]
  vBox <- new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 5]

  -- top buttons

  buttonsBox <- new Gtk.Box
                    [#orientation := Gtk.OrientationHorizontal, #spacing := 5]
  enableAllButton  <- new Gtk.Button [#label := "Enable All"]
  disableAllButton <- new Gtk.Button [#label := "Disable All"]

  #packStart buttonsBox disableAllButton False False 0
  #packEnd buttonsBox enableAllButton False False 0

  #packStart vBox buttonsBox False False 0

  -- disabled attribute list
  disabledStore <- Gtk.listStoreNew [gtypeString]

  disabledList  <- Gtk.treeViewNewWithModel disabledStore
  #setHeadersVisible disabledList False
  #setActivateOnSingleClick disabledList True


  renderer <- new Gtk.CellRendererText []
  column   <- new Gtk.TreeViewColumn []
  #packStart column renderer False
  #addAttribute column renderer "text" 0
  _      <- #appendColumn disabledList column

  select <- #getSelection disabledList
  #setMode select Gtk.SelectionModeSingle

  disabledScroll <- new Gtk.ScrolledWindow
                        [#hscrollbarPolicy := Gtk.PolicyTypeNever]
  #add disabledScroll disabledList

  -- enabled attribute list
  enabledStore <- Gtk.listStoreNew [gtypeString]

  enabledList  <- Gtk.treeViewNewWithModel enabledStore
  #setHeadersVisible enabledList False
  #setActivateOnSingleClick enabledList True

  renderer <- new Gtk.CellRendererText []
  column   <- new Gtk.TreeViewColumn []
  #packStart column renderer False
  #addAttribute column renderer "text" 0
  _      <- #appendColumn enabledList column

  select <- #getSelection enabledList
  #setMode select Gtk.SelectionModeSingle

  enabledScroll <- new Gtk.ScrolledWindow
                       [#hscrollbarPolicy := Gtk.PolicyTypeNever]
  #add enabledScroll enabledList

  -- for_ [disabledList, enabledList] $ \list -> #setReorderable list True

  _ <-
    on enableAllButton #pressed $ moveAll disabledStore enabledStore *> redraw
  _ <-
    on disableAllButton #pressed $ moveAll enabledStore disabledStore *> redraw

  -- horizontal box

  listBox <- new
    Gtk.Box
    [ #orientation := Gtk.OrientationHorizontal
    , #homogeneous := True
    , #spacing := 5
    ]

  #packStart listBox disabledScroll True True 0
  #packEnd listBox enabledScroll True True 0

  #packStart vBox listBox True True 0

  -- toggle transitive dependencies

  transitiveBox <- new
    Gtk.Box
    [#orientation := Gtk.OrientationHorizontal, #spacing := 5]
  transitiveSwitch <- new Gtk.Switch []
  transitiveLabel <- new Gtk.Label [#label := "Include transitive dependencies"]

  #packStart transitiveBox transitiveSwitch False False 0
  #packStart transitiveBox transitiveLabel False False 0

  #packEnd vBox transitiveBox False False 0

  -- interaction


  _ <- on enabledList #rowActivated $ \path _ -> do
    (True, iter) <- #getIter enabledStore path
    withTrans    <- #getState transitiveSwitch
    if withTrans
      then do
        -- TODO: Compute the dependency graph once when a new grammar is loaded
        mayGram <- stateGrammar <$> readIORef stateRef
        for_ mayGram $ \gram -> toggleAttributeTrans (dependencyGraph gram)
                                                     enabledStore
                                                     disabledStore
                                                     iter
      else do
        _ <- toggleAttribute enabledStore disabledStore iter
        return ()
    redraw
  _ <- on disabledList #rowActivated $ \path _ -> do
    (True, iter) <- #getIter disabledStore path
    withTrans    <- #getState transitiveSwitch
    if withTrans
      then do
        mayGram <- stateGrammar <$> readIORef stateRef
        for_ mayGram $ \gram -> toggleAttributeTrans (dependencyGraph gram)
                                                     disabledStore
                                                     enabledStore
                                                     iter
      else do
        _ <- toggleAttribute disabledStore enabledStore iter
        return ()
    redraw

  on filterWindow #deleteEvent $ \_ -> #hideOnDelete filterWindow

  #add filterWindow vBox

  return (filterWindow, disabledStore, enabledStore)

queryTooltip :: IORef State -> Double -> Double -> Gtk.Tooltip -> IO Bool
queryTooltip stateRef x y ttip = do
  mayBBTree <- stateBBTree <$> readIORef stateRef
  fromMaybe (return False) $ do
    bbTree   <- mayBBTree
    attrInfo <- lookupBBTree (x, y) bbTree
    return $ do
      -- Text.putStrLn origin
      #setText ttip $ Just $ Text.strip $ Text.unlines $ case attrInfo of
        TargetInfo tp code origin ->
          [": " <> Text.strip (prettyType tp), origin]
        SourceInfo tp -> [" : " <> prettyType tp]

      return True

setupCodeWindow :: IO (Gtk.Window, Gtk.TextBuffer)
setupCodeWindow = do
  window   <- new Gtk.Window [#typeHint := Gdk.WindowTypeHintDialog]

  textView <- new
    Gtk.TextView
    [ #editable := False
    , #wrapMode := Gtk.WrapModeWordChar
    , #cursorVisible := False
    , #monospace := True
    ]

  #add window textView

  on window #deleteEvent (\_ -> #hideOnDelete window)

  buffer <- get textView #buffer

  return (window, buffer)

activateApp :: Gtk.Application -> IO ()
activateApp app = do
  window     <- Gtk.applicationWindowNew app

  hBox       <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal]

  scroll <- new Gtk.ScrolledWindow [#vscrollbarPolicy := Gtk.PolicyTypeNever]
  area       <- new Gtk.DrawingArea [#hasTooltip := True, #halign := Gtk.AlignCenter]
  sideScroll <- new Gtk.ScrolledWindow
                    [#hscrollbarPolicy := Gtk.PolicyTypeNever]

  stateRef <- newIORef State { stateGrammar      = Nothing
                             , stateSelected     = Nothing
                             , stateHideImplicit = False
                             , stateBBTree       = Nothing
                             }

  (filterWindow, disabledStore, enabledStore) <- setupFilterWindow
    stateRef
    (#queueDraw area)

  (codeWindow  , codeBuffer) <- setupCodeWindow

  (sidebarStore, tree      ) <- setupTree stateRef (#queueDraw area)

  let reader = Reader { readerFilterWindow  = filterWindow
                      , readerCodeWindow    = codeWindow
                      , readerCodeBuffer    = codeBuffer
                      , readerDisabledStore = disabledStore
                      , readerEnabledStore  = enabledStore
                      , readerSidebarStore  = sidebarStore
                      , readerArea          = area
                      , readerState         = stateRef
                      }

  setupMenubar reader app (#queueDraw area)

  #add sideScroll tree
  #packStart hBox sideScroll False False 0

  #add scroll area
  #packStart hBox scroll True True 0
  -- #add vp area

  #add window hBox

  _ <- on area #queryTooltip $ \x y _kbMode ttip ->
    queryTooltip stateRef (fromIntegral x) (fromIntegral y) ttip

  _ <- on area #realize (realize reader)
  _ <- on area #sizeAllocate (sizeAllocate reader)
  _ <- on area #draw (draw reader)

  _ <- on area #buttonPressEvent (buttonPressEvent reader)
  _ <- on area #buttonReleaseEvent (buttonReleaseEvent reader)
  _ <- on area #motionNotifyEvent (motionNotifyEvent reader)

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
