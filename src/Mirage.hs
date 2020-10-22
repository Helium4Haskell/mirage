{-# LANGUAGE OverloadedStrings #-}

module Mirage
  ( Grammar
  , renderGrammar
  , staticInfo
  , StaticInfo(StaticInfo)
  , transitiveClosure
  , dependencyGraph
  , Graph
  , AttrInfo(TargetInfo, SourceInfo)
  , targetInfoType
  , targetInfoCode
  , targetInfoOrigin
  , sourceInfoType
  , BBForest
  , lookupBBForest
  , Type
  , prettyType
  )
where

import           Data.Foldable                  ( traverse_ )
import           Graphics.Rendering.Cairo       ( Render )
import           Data.Text                      ( Text )
import           Data.Set                       ( Set )

import           Mirage.Shape
import           Mirage.Cairo
import           Mirage.AbstractSyntax
import           Mirage.StaticInfo              ( staticInfo
                                                , StaticInfo(StaticInfo)
                                                )
import           Mirage.RenderShapes
import           Mirage.DependencyGraph         ( transitiveClosure
                                                , dependencyGraph
                                                , Graph
                                                )
import           Mirage.CommonTypes             ( Type
                                                , prettyType
                                                )

renderGrammar
  :: (Double, Double)
  -> Grammar
  -> Text
  -> Text
  -> Bool
  -> Set Text
  -> Render (BBForest AttrInfo)
renderGrammar bb gram nont prod hideImplicit enabled = do
  let nodeFont = FontOptions 13 "sans" FontWeightBold
  nodeFontExtents <- mirageFontExtents nodeFont
  let attrFont = FontOptions 13 "sans" FontWeightNormal
  attrFontExtents <- mirageFontExtents attrFont

  cairo           <- askCairo

  let ~(shapes, bbForest) = grammarShapes cairo
                                          nodeFont
                                          nodeFontExtents
                                          attrFont
                                          attrFontExtents
                                          bb
                                          nont
                                          prod
                                          hideImplicit
                                          enabled
                                          gram

  traverse_ renderShape shapes

  return bbForest
