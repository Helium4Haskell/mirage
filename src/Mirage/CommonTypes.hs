{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Mirage.CommonTypes where

import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Aeson
import           GHC.Generics

data Type = Haskell Text
          | NT Text   -- ^ type constructor
               [Text] -- ^ type arguments
          | Self
          deriving (Eq, Ord, Show, Generic)
instance FromJSON Type

prettyType :: Type -> Text
prettyType x = case x of
  Haskell y -> y
  NT y ys   -> Text.unwords (y : ys)
  Self      -> "self"

data Address = Address Text Text deriving (Eq, Ord, Show, Generic)
instance FromJSON Address

addressField :: Address -> Text
addressField (Address x _) = x
