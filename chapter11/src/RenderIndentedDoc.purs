module RenderIndentedDoc where

import Data.Array
import Data.Traversable (sequence)
import Control.Monad.Reader
import Control.Monad.Reader.Class
import Data.String (joinWith)

type Level = Number

type Doc = Reader Level String

line :: String -> Doc
line string = do
  level <- ask
  return $ (indentation level) ++ string
    where
    replicate :: forall a. Number -> a -> [a]
    replicate 0 value = []
    replicate x value = value : replicate (x - 1) value

    indentation :: Number -> String
    indentation x = joinWith "" (replicate x "  ")

indent :: Doc -> Doc
indent = local (+ 1)

cat :: [Doc] -> Doc
cat x = joinWith "\n" <$> sequence x

render :: Doc -> String
render = (`runReader` 0)
