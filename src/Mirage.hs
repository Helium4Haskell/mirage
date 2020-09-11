{-# LANGUAGE OverloadedStrings #-}

module Mirage where

import           Data.Foldable                  ( for_
                                                , traverse_
                                                , fold
                                                )
import           Data.Traversable               ( for )

import qualified Data.Map                      as Map
import           Data.Map                       ( Map )
import           Graphics.Rendering.Cairo       ( Render )
import           Data.Text                      ( Text )
import           Data.Maybe

import           Mirage.Shape
import           Mirage.Cairo

data Rule = Rule
  { ruleParent      :: Node
  , ruleChildren    :: [Node]
  , ruleConnections :: [(Address, Address)]
  }

data Address = Address
  { addressNodeName :: Text
  , addressAttrName :: Maybe Text
  }
  deriving (Eq, Ord, Show)

data Node = Node
  { nodeName :: Text
  , nodeInhs :: [Text]
  , nodeSyns :: [Text]
  }

binRule :: Rule
binRule = Rule
  (Node "lhs" ["i"] ["val"])
  [Node "lt" ["i"] ["val"], Node "rt" ["i"] ["val"]]
  [ (Address "lhs" (Just "i") , Address "lt" (Just "i"))
  , (Address "lt" (Just "val"), Address "lhs" (Just "val"))
  ]

isOrderedExample :: Rule
isOrderedExample = Rule
  (Node "lhs" ["lb", "ub"] ["isOrdered"])
  [ Node "l" ["lb", "ub"] ["isOrdered"]
  , Node "x" []           []
  , Node "r" ["lb", "ub"] ["isOrdered"]
  ]
  [ (Address "lhs" (Just "lb")     , Address "l" (Just "lb"))
  , (Address "lhs" (Just "lb")     , Address "lhs" (Just "isOrdered"))
  , (Address "lhs" (Just "ub")     , Address "r" (Just "ub"))
  , (Address "lhs" (Just "ub")     , Address "lhs" (Just "isOrdered"))
  , (Address "l" (Just "isOrdered"), Address "lhs" (Just "isOrdered"))
  , (Address "r" (Just "isOrdered"), Address "lhs" (Just "isOrdered"))
  , (Address "x" Nothing           , Address "l" (Just "ub"))
  , (Address "x" Nothing           , Address "r" (Just "lb"))
  , (Address "x" Nothing           , Address "lhs" (Just "isOrdered"))
  ]

data NodeRole = NodeRoleParent | NodeRoleChild deriving Eq

data ShapeResult = ShapeResult (Map Address (Double, Double)) [Shape]

instance Semigroup ShapeResult where
  ShapeResult a b <> ShapeResult c d = ShapeResult (a <> c) (b <> d)

instance Monoid ShapeResult where
  mempty = ShapeResult mempty mempty

mirrorShapeResultVertically :: ShapeResult -> ShapeResult
mirrorShapeResultVertically (ShapeResult m x) =
  ShapeResult (Map.map mirrorPointVertically m) (map mirrorShapeVertically x)

translateShapeResult :: (Double, Double) -> ShapeResult -> ShapeResult
translateShapeResult x (ShapeResult a b) =
  ShapeResult (Map.map (translatePoint x) a) (map (translateShape x) b)

data AttrLoc = AttrLocLeft | AttrLocRight deriving Eq

--
--  +------------------------+ ↑
--  |        NodeName        | | nodeNameHeight
--  +                        + ↓
--   \                      /  ↑
--    O inh2          syn2 O   | ↑
--     \                  /    | | attrNameHeight
--      O inh1      syn1 O     | ↓
--       \              /      | 
--        +------------+       ↓ attrNamesheight
--        ←--→←-→
--         w  wspc
nodeShape
  :: Applicative m
  => NodeRole
  -> Node
  -> (FontOptions -> m FontExtents)
  -> (FontOptions -> Text -> m TextExtents)
  -> m ShapeResult
nodeShape role (Node name inhs syns) fontExtents textExtents =
  (\nodeNameFontExtents nodeNameExtents attrNameFontExtents inhExtents synExtents ->
      let w = calculateW (fontExtentsHeight attrNameFontExtents)
                         nodeNameExtents
                         inhExtents
                         synExtents
          n = max (length inhs) (length syns)
          nodeNameHeight =
              ( fontExtentsHeight nodeNameFontExtents
              + fontExtentsVerticalSpacing nodeNameFontExtents
              )
          attrNameHeight = fontExtentsHeight attrNameFontExtents
          attrNamesHeight =
              max 13
                $ attrNameHeight
                * fromIntegral n
                + fontExtentsVerticalSpacing nodeNameFontExtents
      in  fold
            [ trapezoidShape w wspc nodeNameHeight attrNamesHeight n
            , if role == NodeRoleChild
              then ShapeResult
                (Map.singleton (Address name Nothing)
                               (0, attrNamesHeight + nodeNameHeight)
                )
                [ Disk (1, 2 / 5, 0)
                       (0, attrNamesHeight + nodeNameHeight)
                       (0.45 * attrNameHeight)
                ]
              else mempty
            , fold $ map
              (\(an, as) -> diskShapes role
                                       w
                                       wspc
                                       attrNameFontExtents
                                       nodeNameHeight
                                       an
                                       n
                                       as
              )
              [(inhs, AttrLocLeft), (syns, AttrLocRight)]
            , nodeNameShape name nodeNameFontExtents nodeNameExtents
            , fold $ zipWith
              (\as al ->
                attrNameShapes w wspc attrNameFontExtents nodeNameHeight as n al
              )
              (zipWith zip [inhs, syns] [inhExtents, synExtents])
              [AttrLocLeft, AttrLocRight]
            ]
    )
    <$> fontExtents nodeNameOptions
    <*> textExtents nodeNameOptions name
    <*> fontExtents attrNameOptions
    <*> traverse (textExtents attrNameOptions) inhs
    <*> traverse (textExtents attrNameOptions) syns
 where
  -- Note: The attrNameHeight is the same as the horizontal offset of two
  -- consecutive attributes.
  calculateW
    :: Double -> TextExtents -> [TextExtents] -> [TextExtents] -> Double
  calculateW attrNameHeight (TextExtents nodeNameWidth) inhExtents synExtents =
    maximum
      $ (nodeNameWidth / 2)
      : [ attrWidth - i * attrNameHeight
        | attrExtents                <- [inhExtents, synExtents]
        , (i, TextExtents attrWidth) <- zip [0 ..] attrExtents
        ]

  trapezoidShape w wspc nnh anh n = ShapeResult
    mempty
    [ PolyLine
        (0, 0)
        [ (-(anh + wspc + w), 0)
        , (0                , nnh)
        , (anh              , anh)
        , (2 * (w + wspc)   , 0)
        , (anh              , -anh)
        , (0                , -nnh)
        , (-(anh + wspc + w), 0)
        ]
    ]

  -- TODO: merge with attrNameShapes
  diskShapes
    :: NodeRole
    -> Double
    -> Double
    -> FontExtents
    -> Double
    -> [Text]
    -> Int
    -> AttrLoc
    -> ShapeResult
  diskShapes role w wspc attrNameExtents nodeNameHeight attrs n attrLoc = fold
    [ let point =
            ( -al * (wspc + w + (i * hgt) + (vspc + (asc + dsc) / 2 + 1))
            , (fromIntegral n * hgt + vspc)
              + nodeNameHeight
              - (i * hgt)
              - (vspc + (asc + dsc) / 2 + 1)
            )
      in  ShapeResult
            (Map.singleton (Address name (Just attrName)) point)
            [ Disk
                (if (attrLoc == AttrLocLeft) == (role == NodeRoleParent)
                  then (1, 2 / 5, 0)
                  else (0, 2 / 3, 3 / 4)
                )
                point
                (0.45 * hgt)
            ]
    | (i, attrName) <- zip [0 ..] attrs
    ]
   where
    al    = if attrLoc == AttrLocLeft then 1 else -1
    vspc  = fontExtentsVerticalSpacing attrNameExtents
    hgt   = fontExtentsHeight attrNameExtents
    asc   = fontExtentsAscent attrNameExtents
    dsc   = fontExtentsDescent attrNameExtents
    attrN = length attrs

  nodeNameOptions = FontOptions 13 "sans" FontWeightBold
  attrNameOptions = FontOptions 13 "sans" FontWeightNormal

  nodeNameShape name (FontExtents spc _ _) (TextExtents nnw) = ShapeResult
    mempty
    [Text (0, spc) HorizontalAlignCenter VerticalAlignTop nodeNameOptions name]

  attrNameShapes
    :: Double
    -> Double
    -> FontExtents
    -> Double
    -> [(Text, TextExtents)]
    -> Int
    -> AttrLoc
    -> ShapeResult
  attrNameShapes w wspc attrNameExtents nodeNameHeight attr n attrLoc =
    ShapeResult
      mempty
      [ Text
          ( -al
            * (wspc + w + (i * hgt) - if attrLoc == AttrLocRight
                then attrWidth
                else 0
              )
          , (fromIntegral n * hgt + vspc)
          + nodeNameHeight
          - (i * hgt)
          - (vspc + 1)
          )
          HorizontalAlignLeft
          VerticalAlignBottom
          attrNameOptions
          attrName
      | (i, (attrName, TextExtents attrWidth)) <- zip [0 ..] attr
      ]
   where
    al                       = if attrLoc == AttrLocLeft then 1 else -1
    FontExtents vspc dsc asc = attrNameExtents
    hgt                      = fontExtentsHeight attrNameExtents

  wspc = 13

renderRule :: Rule -> Render ()
renderRule (Rule x xs cs) = do
  ShapeResult m1 shs1 <-
    translateShapeResult (500, 100)
      <$> nodeShape NodeRoleParent x mirageFontExtents mirageTextExtents

  ShapeResult m2 shs2 <- fmap fold . for (zip [0 ..] xs) $ \(i, node) ->
    translateShapeResult
        (500 + (i - fromIntegral (length xs - 1) / 2) * 200, 700)
      .   mirrorShapeResultVertically
      <$> nodeShape NodeRoleChild node mirageFontExtents mirageTextExtents

  let m   = m1 <> m2
      shs = shs1 <> shs2

  let lines = mapMaybe
        (\(from, to) -> case (Map.lookup from m, Map.lookup to m) of
          (Just (x, y), Just (x', y')) -> do
            Just (Bezier (x, y) (x, 400) (x', 400) (x', y'))
          _ -> Nothing
        )
        cs

  traverse_ renderShape (lines <> shs)
