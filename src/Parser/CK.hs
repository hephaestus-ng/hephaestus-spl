module Parser.CK where

import Text.Parsec

import Data.SPL
import Data.FM.Expression

parseCK :: (Asset a) => Parsec String () (ConfigurationKnowledge a)
parseCK =  many1 parseItem 

parseItem :: (Asset a) => Parsec String () ((FeatureExp, [Transformation a]))
parseItem = parseExp      >>= \exp -> many space
         >> string "=>"   >> many space  
         >> string "["    >> many space
         >> parseTransformations >>= \ts -> many space
         >> string "]"    >> many (space <|> newline <|> tab)
         >> return (exp, ts)
                                       

parseExp = undefined

parseTransformations :: (Asset a) => Parsec String () [Transformation a]
parseTransformations =
  (parserT >>= \t -> many space >> char ',' >> parseTransformations >>= \ts -> return (t:ts)) <|>
  (parserT >>= \t -> many space >> return [t]) 
                       
