module Test.CKTest where

import Data.SPL
import Text.Parsec
import Text.Parsec.String
import Parser.CK
-- import Data.SourceCode


data HwAsset = HwAsset {
  msg :: String
} deriving (Show)

instance Asset HwAsset where
  initialize = Product $ HwAsset { msg = "Hello World" }
  parserT    = hwParser


-- Transformation
setMessage :: String -> Transformation HwAsset
setMessage s _ (Product p) = Product $ p { msg = s }


-- ParserT -> this would go on Parser/Transformation.hs
hwParser :: Parsec String () (Transformation HwAsset)
hwParser = string "setMessage(\"" >> many1 letter >>= \s -> string "\")" >> return (setMessage s)


parserTest = parseFromFile (parseCK :: (Parsec String () (ConfigurationKnowledge HwAsset))) "/home/thi4go/hephaestus-ng/hephaestus-spl/test/test.ck" >>= \result ->
  case result of
    Left err -> print err
    Right ((e, ts):xs) -> print e
