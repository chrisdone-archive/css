{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
-- | CSS generation.

module Language.CSS
  (module Language.CSS.Types
  ,module Language.CSS.Properties
  ,runCSS
  ,renderCSS
  ,renderPrettyCSS
  ,rules
  ,rule)
    where

import           Language.CSS.Properties
import           Language.CSS.Types

import           Control.Monad.Writer    (MonadWriter,runWriter,tell)
import           Data.Either             (lefts,rights)
import           Data.Monoid             (Monoid(..))
import           Data.Monoid.Operator    ((++))
import           Data.Text.Lazy          (Text)
import qualified Data.Text.Lazy          as T
import           Prelude                 hiding ((++))

-- | Generate CSS rules.
runCSS :: CSS Rule -> [Rule]
runCSS = snd . runWriter . unCSS

-- | Generate CSS properties.
runBody :: CSS (Either Property Rule) -> [(Either Property Rule)]
runBody = snd . runWriter . unCSS

-- | Render a CSS AST to text, flat.
renderCSS :: [Rule] -> Text
renderCSS = mconcat . map renderRule where
  renderRule (Rule _name [] []) = ""
  renderRule (Rule name props sub) =
    parent ++
    renderCSS (map prefix sub)
      where parent | null props = ""
                   | otherwise = name ++ "{" ++ renderProps props ++ "}"
            prefix subr@Rule{ruleExpr} =
              subr { ruleExpr = name ++ " " ++ ruleExpr }
  renderProps = T.intercalate ";" . map renderProp
  renderProp (Property name value) = name ++ ":" ++ value

-- | Render a CSS AST to text, pretty.
renderPrettyCSS :: [Rule] -> Text
renderPrettyCSS = mconcat . map renderRule where
  renderRule (Rule name props sub) =
    name ++ "{\n" ++ renderProps props ++ "\n}" ++ "\n" ++
    renderPrettyCSS (map prefix sub)
      where prefix subr@Rule{ruleExpr} =
              subr { ruleExpr = name ++ " " ++ ruleExpr }
  renderProps = T.intercalate ";\n" . map (("    "++) . renderProp)
  renderProp (Property name value) = name ++ ": " ++ value

class Ruleable a where
  rule :: Text -> CSS (Either Property Rule) -> CSS a
  rules :: [Text] -> CSS (Either Property Rule) -> CSS a
  rules rs body = mapM_ (`rule` body) rs

instance Ruleable Rule where
  rule name getProps = do
    let body = runBody getProps
    tell $ [Rule name (lefts body) (rights body)]

instance Ruleable (Either Property Rule) where
  rule name getProps = do
    let body = runBody getProps
    tell $ [Right $ Rule name (lefts body) (rights body)]
