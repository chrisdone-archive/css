{-# OPTIONS -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | CSS generation.

module Language.CSS.Types where

import Control.Monad.Writer (Writer,MonadWriter)
import Data.Text.Lazy       (Text)

-- | The CSS writer.
newtype CSSM x a = CSSM { unCSS :: Writer [x] a }
  deriving (Functor,Monad,MonadWriter [x])

type CSS x = CSSM x ()

-- | A CSS rule.
data Rule = Rule { ruleExpr :: Text
                 , ruleProperties :: [Property]
                 , ruleRules :: [Rule]
                 }
  deriving Show

-- | A CSS property.
data Property = Property { propertyName :: Text
                         , propertyValue :: Text
                         }
  deriving Show
