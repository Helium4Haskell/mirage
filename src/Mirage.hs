{-# LANGUAGE OverloadedStrings #-}

module Mirage
  ( Grammar
  , renderGrammar
  , staticInfo
  , StaticInfo(StaticInfo)
  , transitiveClosure
  , dependencyGraph
  , Graph
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
import           Mirage.CommonTypes             ( )

{-
binExample :: Grammar
binExample = Grammar
  [ Nonterminal
      "Bin"
      ["a"]
      [Attribute "i" (Haskell "Int")]
      [Attribute "val" (Haskell "@a")]
      [ Production
        "Bin"
        [ Child "l" (NT "Bin" [])
        , Child "x" (Haskell "@a")
        , Child "r" (NT "Bin" [])
        ]
        []
      , Production "Leaf" [] []
      ]
  ]

isOrderedExample :: Production
isOrderedExample = Production
  (Node "lhs" ["lb", "ub"] ["isOrdered"])
  [ Node "l" ["lb", "ub"] ["isOrdered"]
  , Node "x" []           []
  , Node "r" ["lb", "ub"] ["isOrdered"]
  ]
  ["isOrdered"]
  [ Connection (Address "lhs" (Just "lb")) (Address "l" (Just "lb"))
  , Connection (Address "lhs" (Just "lb")) (Address "loc" (Just "isOrdered"))
  , Connection (Address "lhs" (Just "ub")) (Address "r" (Just "ub"))
  , Connection (Address "lhs" (Just "ub")) (Address "loc" (Just "isOrdered"))
  , Connection (Address "l" (Just "isOrdered"))
               (Address "loc" (Just "isOrdered"))
  , Connection (Address "r" (Just "isOrdered"))
               (Address "loc" (Just "isOrdered"))
  , Connection (Address "x" Nothing) (Address "l" (Just "ub"))
  , Connection (Address "x" Nothing) (Address "r" (Just "lb"))
  , Connection (Address "x" Nothing) (Address "loc" (Just "isOrdered"))
  , Connection (Address "loc" (Just "isOrdered"))
               (Address "lhs" (Just "isOrdered"))
  ]
-}

renderGrammar
  :: (Double, Double) -> Grammar -> Text -> Text -> Bool -> Set Text -> Render ()
renderGrammar bb gram nont prod hideImplicit enabled = do
  let nodeFont = FontOptions 13 "sans" FontWeightBold
  nodeFontExtents <- mirageFontExtents nodeFont
  let attrFont = FontOptions 13 "sans" FontWeightNormal
  attrFontExtents <- mirageFontExtents attrFont

  cairo           <- askCairo

  let shapes = grammarShapes cairo
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
