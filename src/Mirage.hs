{-# LANGUAGE OverloadedStrings #-}

module Mirage where

import           Data.Foldable                  ( traverse_
                                                , fold
                                                )
import           Data.Traversable               ( for )

import qualified Data.Map                      as Map
import           Data.Map                       ( Map )
import           Graphics.Rendering.Cairo       ( Render )
import           Data.Text                      ( Text )
import           Data.Maybe                     ( mapMaybe )

import           Mirage.Shape
import           Mirage.Cairo
import           Mirage.AG

binRule :: Rule
binRule = Rule
  (Node "lhs" ["i"] ["val"])
  [Node "lt" ["i"] ["val"], Node "rt" ["i"] ["val"]]
  []
  [ Connection (Address "lhs" (Just "i"))  (Address "lt" (Just "i"))
  , Connection (Address "lt" (Just "val")) (Address "lhs" (Just "val"))
  ]

isOrderedExample :: Rule
isOrderedExample = Rule
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

renderRule :: (Double, Double) -> Rule -> Render ()
renderRule bb rule = do
  let nodeFont = FontOptions 13 "sans" FontWeightBold
  nodeFontExtents <- mirageFontExtents nodeFont
  let attrFont = FontOptions 13 "sans" FontWeightNormal
  attrFontExtents <- mirageFontExtents attrFont

  cairo           <- askCairo

  traverse_
    renderShape
    (ruleShapes cairo nodeFont nodeFontExtents attrFont attrFontExtents bb rule)
