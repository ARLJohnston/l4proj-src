module LTLParser (runLTLParser) where

import Text.Parsec
import LTL

type LTLParser a = Parsec String [([Char], [Bool])] a

runLTLParser :: String -> [([Char], [Bool])] -> Either ParseError LTLFormula
runLTLParser input lookupTable = runParser ltlParser lookupTable "LTL Parser" input

getKeys :: [([Char], a)] -> [[Char]]
getKeys [] = []
getKeys ((s, _):xs) = s : getKeys xs

-- Main Parser
ltlParser :: LTLParser LTLFormula
ltlParser = do
  phi <- start
  maybePsi <- end ["^"]
  case maybePsi of
    Nothing -> return phi
    Just psi -> return (LTLAnd phi psi)

-- Non-Recursive parser
labelParser :: LTLParser LTLFormula
labelParser = do
  lookupTable <- getState
  spaces
  value <- choice $ map (try . string) $ getKeys lookupTable
  case lookup value lookupTable of
    Just sat -> return $ LTLLabel sat
    Nothing -> fail "Unable to match any given SAT values"

-- Start parsers
start :: LTLParser LTLFormula
start =
      untilParser
  <|> labelParser
  <|> groupParser
  <|> notParser
  <|> nextParser

end :: [[Char]] -> LTLParser (Maybe LTLFormula)
end op =
      getEnd
  <|> return Nothing
  where
    getEnd = do
      spaces
      choice $ map (try . string) op
      spaces
      Just <$> ltlParser

-- Prefix Parsers
groupParser :: LTLParser LTLFormula
groupParser = do
  spaces
  char '('
  phi <- ltlParser
  char ')'
  return phi

notParser :: LTLParser LTLFormula
notParser = do
  spaces
  choice $ map (try . string) ["¬", "not", "!"]
  LTLNot <$> ltlParser

nextParser :: LTLParser LTLFormula
nextParser = do
  spaces
  choice $ map (try . string) ["X", "Next"]
  Next <$> ltlParser

untilParser :: LTLParser LTLFormula
untilParser = do
  phi <- ltlParser
  maybePsi <- end ["U", "until"]
  case maybePsi of
    Nothing -> error "No psi specified for until clause"
    Just psi -> return (Until phi psi)
