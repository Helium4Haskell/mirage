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
import           Data.Maybe

import           Mirage.Shape
import           Mirage.Cairo

nodeNameOptions, attrNameOptions :: FontOptions
nodeNameOptions = FontOptions 13 "sans" FontWeightBold
attrNameOptions = FontOptions 13 "sans" FontWeightNormal

data Rule = Rule
  { ruleParent      :: Node
  , ruleChildren    :: [Node]
  , ruleLocals      :: [Text]
  , ruleConnections :: [(Address, Address)]
  }

data Address = Address
  { addressNodeName :: Text
  -- ^ 'lhs' and 'loc' are special. They must always have an attribute name
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
  []
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
  ["isOrdered"]
  [ (Address "lhs" (Just "lb")       , Address "l" (Just "lb"))
  , (Address "lhs" (Just "lb")       , Address "loc" (Just "isOrdered"))
  , (Address "lhs" (Just "ub")       , Address "r" (Just "ub"))
  , (Address "lhs" (Just "ub")       , Address "loc" (Just "isOrdered"))
  , (Address "l" (Just "isOrdered")  , Address "loc" (Just "isOrdered"))
  , (Address "r" (Just "isOrdered")  , Address "loc" (Just "isOrdered"))
  , (Address "x" Nothing             , Address "l" (Just "ub"))
  , (Address "x" Nothing             , Address "r" (Just "lb"))
  , (Address "x" Nothing             , Address "loc" (Just "isOrdered"))
  , (Address "loc" (Just "isOrdered"), Address "lhs" (Just "isOrdered"))
  ]

data NodeRole = NodeRoleParent | NodeRoleChild deriving Eq

data ShapeResult = ShapeResult (Map Address (Double, Double))
                               (Map Address (Double, Double))
                               [Shape]

instance Semigroup ShapeResult where
  ShapeResult a b c <> ShapeResult d e f =
    ShapeResult (a <> d) (b <> e) (c <> f)

instance Monoid ShapeResult where
  mempty = ShapeResult mempty mempty mempty

mirrorShapeResultVertically :: ShapeResult -> ShapeResult
mirrorShapeResultVertically (ShapeResult src tgt x) = ShapeResult
  (Map.map mirrorPointVertically src)
  (Map.map mirrorPointVertically tgt)
  (map mirrorShapeVertically x)

translateShapeResult :: (Double, Double) -> ShapeResult -> ShapeResult
translateShapeResult x (ShapeResult a b c) = ShapeResult
  (Map.map (translatePoint x) a)
  (Map.map (translatePoint x) b)
  (map (translateShape x) c)

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
            [ trapezoidShape w nodeNameHeight attrNamesHeight
            , if role == NodeRoleChild
              then ShapeResult
                (Map.singleton (Address name Nothing)
                               (0, attrNamesHeight + nodeNameHeight)
                )
                (Map.singleton (Address name Nothing)
                               (0, -(attrNamesHeight + nodeNameHeight))
                )
                [ Disk (1, 2 / 5, 0)
                       (0, attrNamesHeight + nodeNameHeight)
                       (0.45 * attrNameHeight)
                ]
              else mempty
            , fold $ map
              (\(an, as) ->
                diskShapes w attrNameFontExtents nodeNameHeight an n as
              )
              [(inhs, AttrLocLeft), (syns, AttrLocRight)]
            , nodeNameShape nodeNameFontExtents
            , fold $ zipWith
              (\as al ->
                attrNameShapes w attrNameFontExtents nodeNameHeight as n al
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

  trapezoidShape w nnh anh = ShapeResult
    mempty
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
    :: Double
    -> FontExtents
    -> Double
    -> [Text]
    -> Int
    -> AttrLoc
    -> ShapeResult
  diskShapes w attrNameExtents nodeNameHeight attrs n attrLoc = fold
    [ let point =
            ( -al * (wspc + w + (i * hgt) + (vspc + (asc + dsc) / 2 + 1))
            , (fromIntegral n * hgt + vspc)
              + nodeNameHeight
              - (i * hgt)
              - (vspc + (asc + dsc) / 2 + 1)
            )
      in  if (attrLoc == AttrLocLeft) == (role == NodeRoleParent)
            then ShapeResult
              (Map.singleton (Address name (Just attrName)) point)
              mempty
              [Disk (1, 2 / 5, 0) point (0.45 * hgt)]
            else ShapeResult
              mempty
              (Map.singleton (Address name (Just attrName)) point)
              [Disk (0, 2 / 3, 3 / 4) point (0.45 * hgt)]
    | (i, attrName) <- zip [0 ..] attrs
    ]
   where
    al   = if attrLoc == AttrLocLeft then 1 else -1
    vspc = fontExtentsVerticalSpacing attrNameExtents
    hgt  = fontExtentsHeight attrNameExtents
    asc  = fontExtentsAscent attrNameExtents
    dsc  = fontExtentsDescent attrNameExtents

  nodeNameShape (FontExtents spc _ _) = ShapeResult
    mempty
    mempty
    [Text (0, spc) HorizontalAlignCenter VerticalAlignTop nodeNameOptions name]

  attrNameShapes
    :: Double
    -> FontExtents
    -> Double
    -> [(Text, TextExtents)]
    -> Int
    -> AttrLoc
    -> ShapeResult
  attrNameShapes w attrNameExtents nodeNameHeight attr n attrLoc = ShapeResult
    mempty
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
    al                   = if attrLoc == AttrLocLeft then 1 else -1
    FontExtents vspc _ _ = attrNameExtents
    hgt                  = fontExtentsHeight attrNameExtents

  wspc = 13

--
--       +---O---+
--      /         \
--     +           +
--     |  LocName  |
--     +           +
--      \         /
--       +---O---+
--
locShape
  :: Applicative m
  => Text
  -> (FontOptions -> m FontExtents)
  -> (FontOptions -> Text -> m TextExtents)
  -> m ShapeResult
locShape name fontExtents textExtents =
  (\nameFontExtents nameExtents ->
      let
        w = textExtentsWidth nameExtents / 2
        nnh =
          ( fontExtentsHeight nameFontExtents
          + fontExtentsVerticalSpacing nameFontExtents
          )
        anh  = 13
        wspc = 13
      in
        ShapeResult
          (Map.singleton (Address "loc" (Just name)) (0, -(anh + nnh / 2)))
          (Map.singleton (Address "loc" (Just name)) (0, anh + nnh / 2))
          [ PolyLine
            (-(anh + wspc + w), 0)
            [ (0              , nnh / 2)
            , (anh            , anh)
            , (2 * (w + wspc) , 0)
            , (anh            , -anh)
            , (0              , -nnh)
            , (-anh           , -anh)
            , (-2 * (w + wspc), 0)
            , (-anh           , anh)
            , (0              , nnh / 2)
            ]
          , Text (0, 0)
                 HorizontalAlignCenter
                 VerticalAlignCenter
                 nodeNameOptions
                 name
          , Disk (1, 2 / 5, 0)     (0, -(anh + nnh / 2)) 6
          , Disk (0, 2 / 3, 3 / 4) (0, anh + nnh / 2)    6
          ]
    )
    <$> fontExtents nodeNameOptions
    <*> textExtents nodeNameOptions name


renderRule :: Rule -> Render ()
renderRule (Rule x xs ls cs) = do
  ShapeResult src1 tgt1 shs1 <-
    translateShapeResult (500, 100)
      <$> nodeShape NodeRoleParent x mirageFontExtents mirageTextExtents

  ShapeResult src2 tgt2 shs2 <- fmap fold . for (zip [0 ..] xs) $ \(i, node) ->
    translateShapeResult
        (500 + (i - fromIntegral (length xs - 1) / 2) * 200, 700)
      .   mirrorShapeResultVertically
      <$> nodeShape NodeRoleChild node mirageFontExtents mirageTextExtents

  ShapeResult src3 tgt3 shs3 <- fold <$> traverse
    (\(i, l) ->
      translateShapeResult (700 + 200 * i, 200)
        <$> locShape l mirageFontExtents mirageTextExtents
    )
    (zip [0 ..] ls)

  let src = src1 <> src2 <> src3
      tgt = tgt1 <> tgt2 <> tgt3
      shs = shs1 <> shs2 <> shs3

  let
    connections = mapMaybe
      (\(from@(Address fromNode _), to@(Address toNode _)) ->
        case (Map.lookup from src, Map.lookup to tgt) of
          (Just (x, y), Just (x', y')) -> do
            Just
              (Bezier
                (x, y)
                (x, if fromNode == "lhs" then y + 200 else y - 200)
                ( x'
                , if toNode `elem` ["lhs", "loc"] then y' + 200 else y' - 200
                )
                (x', y')
              )
          _ -> Nothing
      )
      cs

  traverse_ renderShape (connections <> shs)
