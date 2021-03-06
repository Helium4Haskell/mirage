{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Mirage.Cairo where

import           Graphics.Rendering.Cairo
                                         hiding ( x
                                                , y
                                                )
import qualified Mirage.Shape                  as Mirage
import           Mirage.Shape                   ( Shape(..)
                                                , Color(..)
                                                , verticalAlignOffset
                                                , horizontalAlignOffset
                                                )
import           Data.Text                      ( Text )
import           Data.Foldable                  ( traverse_ )

setSourceColor :: Color -> Render ()
setSourceColor (RGB r g b   ) = setSourceRGB r g b
setSourceColor (RGBA r g b a) = setSourceRGBA r g b a

renderShape :: Shape -> Render ()
renderShape (PolyLine x xs) = do
  save
  uncurry moveTo x
  traverse_ (uncurry relLineTo) xs
  setSourceRGB 1 1 1
  fill
  restore
  uncurry moveTo x
  traverse_ (uncurry relLineTo) xs
  stroke
renderShape (Disk color (x, y) radius) = do
  save
  moveTo 0 0
  setSourceColor color
  arc x y radius 0 (2 * pi)
  fill
  restore
renderShape (Text (x, y) ha va f@(Mirage.FontOptions sz face wgt) txt) = do
  Mirage.FontExtents _ dsc asc <- mirageFontExtents f
  Mirage.TextExtents w         <- mirageTextExtents f txt
  let h = dsc + asc
  moveTo (x - w * horizontalAlignOffset ha)
         (y - dsc + h * verticalAlignOffset va)
  setFontSize sz
  selectFontFace @Text face FontSlantNormal (mirageFontWeightCairo wgt)
  showText txt
  stroke
renderShape (Bezier color (x1, y1) (x2, y2) (x3, y3) (x4, y4)) = do
  save
  moveTo x1 y1
  setSourceColor color
  curveTo x2 y2 x3 y3 x4 y4
  stroke
  restore

mirageFontWeightCairo :: Mirage.FontWeight -> FontWeight
mirageFontWeightCairo w = case w of
  Mirage.FontWeightNormal -> FontWeightNormal
  Mirage.FontWeightBold   -> FontWeightBold

mirageFontExtents :: Mirage.FontOptions -> Render Mirage.FontExtents
mirageFontExtents (Mirage.FontOptions sz face wgt) = do
  save
  setFontSize sz
  selectFontFace @Text face FontSlantNormal (mirageFontWeightCairo wgt)
  FontExtents a d h _ _ <- fontExtents
  restore
  let ascspc = h - sz
      asc    = a - ascspc
      dsc    = d - d * ascspc / a
      hgt    = h + 1
  return (Mirage.FontExtents (hgt - asc - dsc) dsc asc)

mirageTextExtents :: Mirage.FontOptions -> Text -> Render Mirage.TextExtents
mirageTextExtents (Mirage.FontOptions sz face wgt) txt = do
  save
  setFontSize sz
  selectFontFace @Text face FontSlantNormal (mirageFontWeightCairo wgt)
  TextExtents _ _ w _ _ _ <- textExtents txt
  restore
  return (Mirage.TextExtents w)
