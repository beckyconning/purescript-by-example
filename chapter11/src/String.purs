module String where

import Control.Alt
import Control.Alternative
import Control.Monad.Error
import Control.Monad.Error.Class
import Control.Monad.Error.Trans
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.State.Class
import Control.Monad.State.Trans
import Control.Monad.Trans
import Control.Monad.Writer
import Control.Monad.Writer.Class
import Control.Monad.Writer.Trans
import Data.Either
import Data.String hiding (split)
import Data.Tuple

type Parser = StateT String (WriterT [String] (ErrorT String Identity))

runParser p s = runIdentity $ runErrorT $ runWriterT $ runStateT p s

split :: Parser String
split = do
  s <- get
  lift $ tell ["The state is " ++ show s]
  case s of
    "" -> lift $ lift $ throwError "Empty string"
    _ -> do
      put (drop 1 s)
      return (take 1 s)

string :: String -> Parser String
string prefix = do
  s <- get
  tell ["The state is " ++ show s]
  case (f s) of
    x | x == prefix -> do
      put (g s)
      return prefix
    _      -> throwError $ prefix ++ " is not a prefix of " ++ s
    where
    f :: String -> String
    f = take (length prefix)
    g :: String -> String
    g = drop (length prefix)

isAsThenBs :: String -> Boolean
isAsThenBs = interpretResult <<< runParser (f "a" >>= \_ -> f "b")
  where
  f :: String -> Parser [String]
  f = many <<< string

  hasAtLeastTwoElements :: forall a. [a] -> Boolean
  hasAtLeastTwoElements = (< 2) <<< Data.Array.length

  interpretResult :: Either String (Tuple (Tuple [String] String) [String]) -> Boolean
  interpretResult (Left _)                                                             = false
  interpretResult (Right (Tuple (Tuple xs y) _)) | hasAtLeastTwoElements xs || y /= "" = false
  interpretResult _                                                                    = true

isAsAndBs :: String -> Boolean
isAsAndBs = interpretResult <<< runParser (many aOrB)
  where
  aOrB :: Parser String
  aOrB = string "a" <|> string "b"

  interpretResult :: Either String (Tuple (Tuple [String] String) [String]) -> Boolean
  interpretResult (Right (Tuple (Tuple [] _) _)) = false
  interpretResult (Right (Tuple (Tuple _ "") _)) = true
  interpretResult _                              = false

-- type Parser = ErrorT String (StateT String (WriterT [String] Identity))
-- requires runparser's subexpressions to be rearranged
-- changes the co-domain of runParser to Tuple (Tuple (Maybe String) String) [String]
-- there isn't an instance of Lazy1 for ErrorT so many won't work
