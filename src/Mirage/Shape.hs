module Mirage.Shape where

import           Data.Text                      ( Text )

-- For the vertical and horizontal alignment we consider the bounding box of the
-- text to be the width of the text as reported by cairo's textExtents function
-- and the height of the text as the ascent plus the descent as reported by
-- cairo's fontExtents function. All the inked pixels are expected to fall
-- inside this bounding box, but extra spacing is necessary to properly display
-- the text.

data VerticalAlign
  = VerticalAlignCenter
  | VerticalAlignBottom
  | VerticalAlignTop
  deriving Eq

-- | Calculate the relative vertical offset from the (standard) bottom left
-- corner of the text bounding box.
verticalAlignOffset :: VerticalAlign -> Double
verticalAlignOffset x = case x of
  VerticalAlignBottom -> 0
  VerticalAlignCenter -> 1 / 2
  VerticalAlignTop    -> 1

data HorizontalAlign
  = HorizontalAlignLeft
  | HorizontalAlignCenter
  | HorizontalAlignRight
  deriving Eq

-- | Calculate the relative horizontal offset from the (standard) bottom left
-- corner of the text bounding box.
horizontalAlignOffset :: HorizontalAlign -> Double
horizontalAlignOffset x = case x of
  HorizontalAlignLeft   -> 0
  HorizontalAlignCenter -> 1 / 2
  HorizontalAlignRight  -> 1

data Shape
  = PolyLine (Double, Double) -- ^ origin
             [(Double, Double)] -- ^ offsets
  | Disk (Double, Double, Double) (Double, Double) Double
  | Text (Double, Double) HorizontalAlign VerticalAlign FontOptions Text
  | Bezier (Double, Double) (Double, Double) (Double, Double) (Double, Double)

mirrorPointVertically :: (Double, Double) -> (Double, Double)
mirrorPointVertically (x, y) = (x, -y)

mirrorVerticalAlignVertically :: VerticalAlign -> VerticalAlign
mirrorVerticalAlignVertically x = case x of
  VerticalAlignTop    -> VerticalAlignBottom
  VerticalAlignCenter -> VerticalAlignCenter
  VerticalAlignBottom -> VerticalAlignTop

mirrorShapeVertically :: Shape -> Shape
mirrorShapeVertically (PolyLine x xs) =
  PolyLine (mirrorPointVertically x) (map mirrorPointVertically xs)
mirrorShapeVertically (Disk color x r) = Disk color (mirrorPointVertically x) r
mirrorShapeVertically (Text x ha va font txt) =
  Text (mirrorPointVertically x) ha (mirrorVerticalAlignVertically va) font txt

translatePoint :: (Double, Double) -> (Double, Double) -> (Double, Double)
translatePoint (x', y') (x, y) = (x + x', y + y')

translateShape :: (Double, Double) -> Shape -> Shape
translateShape x' (PolyLine x xs ) = PolyLine (translatePoint x' x) xs
translateShape x' (Disk color x r) = Disk color (translatePoint x' x) r
translateShape x' (Text x ha va font txt) =
  Text (translatePoint x' x) ha va font txt

data FontWeight = FontWeightNormal | FontWeightBold deriving (Eq, Show)

data FontOptions = FontOptions
  { fontOptionsSize     :: Double
  , fontOptionsFontFace :: Text
  , fontOptionsWeight   :: FontWeight
  }

data FontExtents = FontExtents
  { fontExtentsVerticalSpacing :: Double
  , fontExtentsDescent         :: Double
  , fontExtentsAscent          :: Double
  }

fontExtentsHeight :: FontExtents -> Double
fontExtentsHeight (FontExtents vs d a) = vs + d + a

data TextExtents = TextExtents
  { textExtentsWidth :: Double
  }
