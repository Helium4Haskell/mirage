module Mirage.Instances where

import           Mirage.AbstractSyntax
import           Data.Aeson

instance FromJSON Grammar
instance FromJSON Nonterminal
instance FromJSON Attribute
instance FromJSON Production
instance FromJSON Child
instance FromJSON Rule
