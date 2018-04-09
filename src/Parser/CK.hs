module Parser.CK where

import Text.Parsec

import Data.Expression

import Data.SPL

parseCK :: (Asset a) => Parsec String () (ConfigurationKnowledge a)
parseCK = undefined

parseItem :: (Asset a) => Parsec String () ((FeatureExp, [Transformation a]))
parseItem = parseExp      >>= \exp -> many space
         >> string "=> "  >> many space  
         >> string "["    >> many space
         >> parseTransformations >>= \ts -> many space
         >> string "]" >> return (exp, ts)
                                       

parseExp = undefined

parseTransformations :: (Asset a) => Parsec String () [Transformation a]
parseTransformations =  (parseT >>= \t -> many space >> ((char ',') >> parseTransformations >>= \ts -> return (t:ts))
                    <|> (parseT >>= \t -> many space >> return [t] 
                       
