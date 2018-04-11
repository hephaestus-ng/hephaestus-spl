module Parser.CK where

import Text.Parsec

import Data.SPL
import Data.FM.Types
import Parser.Expression

parseCK :: (Asset a) => Parsec String () (ConfigurationKnowledge a)
parseCK =  many1 parseItem

parseItem :: (Asset a) => Parsec String () (ConfigurationItem a)
parseItem = parseExpression      >>= \exps -> blanks
         >> string "=>"          >> blanks
         >> string "["           >> blanks
         >> parseTransformations >>= \ts -> blanks
         >> string "]"           >> blanks
         >> return (exps, ts)


blanks = many (space <|> newline <|> tab)

parseTransformations :: (Asset a) => Parsec String () [Transformation a]
parseTransformations =
  try (parserT >>= \t -> char ',' >> blanks >> parseTransformations >>= \ts -> return (t:ts))
  <|>
  (parserT >>= \t -> blanks >> return [t])
