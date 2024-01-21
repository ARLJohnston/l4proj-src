module CTLParser (runCTLParser) where

import Text.Parsec
import CTL

type CTLParser a = Parsec String [([Char], CTLFormula)] a

runCTLParser :: String -> [([Char], CTLFormula)] -> Either ParseError CTLFormula
runCTLParser input lookupTable = runParser ctlParser lookupTable "CTL Parser" input

getKeys :: [([Char], a)] -> [[Char]]
getKeys [] = []
getKeys ((s, _):xs) = s : getKeys xs

-- Main Parser
ctlParser :: CTLParser CTLFormula
ctlParser = do
  phi <- start
  maybePsi <- end
  case maybePsi of
    Nothing -> return phi
    Just psi -> return (And phi psi)

-- Non-Recursive parser
satisfactionParser :: CTLParser CTLFormula
satisfactionParser = do
  lookupTable <- getState
  spaces
  value <- choice $ map try $ map string $ getKeys lookupTable
  case lookup value lookupTable of
    Just sat -> return sat
    Nothing -> fail "Unable to match any given SAT values"

-- Start parsers
start :: CTLParser CTLFormula
start =
      satisfactionParser
  <|> groupParser
  <|> notParser
  <|> existsNextParser
  <|> existsAlwaysParser
  <|> forAllNextParser
  <|> forAllEventuallyParser
  <|> forAllAlwaysParser
  <|> untilParser

end :: CTLParser (Maybe CTLFormula)
end =
      getEnd
  <|> return Nothing
  where
    getEnd = do
      spaces
      char '^'
      spaces
      psi <- ctlParser
      return (Just psi)

untilParser :: CTLParser CTLFormula
untilParser = do
  expr1 <- startUntil
  maybeExpr2 <- endUntil
  case maybeExpr2 of
    Nothing -> error "No psi specified for until"
    Just psi -> case expr1 of
      ForAllPhiUntilPsi phi _ -> return (ForAllPhiUntilPsi phi psi)
      ExistsPhiUntilPsi phi _ -> return (ExistsPhiUntilPsi phi psi)

startUntil :: CTLParser CTLFormula
startUntil =
      forAllParser
  <|> existsParser

endUntil :: CTLParser (Maybe CTLFormula)
endUntil =
      getEndUntil
  <|> return Nothing
  where
    getEndUntil = do
      spaces
      char 'U'
      spaces
      psi <- ctlParser
      return (Just psi)

-- Prefix Parsers
groupParser :: CTLParser CTLFormula
groupParser = do
  spaces
  char '('
  phi <- ctlParser
  char ')'
  return phi

forAllParser :: CTLParser CTLFormula
forAllParser = do
  spaces
  try (string "∀") <|> try (string "always")
  phi <- ctlParser
  return (ForAllPhiUntilPsi phi phi)

existsParser :: CTLParser CTLFormula
existsParser = do
  spaces
  try (string "∃") <|> try (string "exists")
  phi <- ctlParser
  return (ExistsPhiUntilPsi phi phi)

notParser :: CTLParser CTLFormula
notParser = do
  spaces
  try (string "¬") <|> try (string "not")
  phi <- ctlParser
  return (Not phi)

existsNextParser :: CTLParser CTLFormula
existsNextParser = do
  spaces
  try (string "∃X") <|> try (string "existsNext")
  phi <- ctlParser
  return (ExistsNext phi)

existsAlwaysParser :: CTLParser CTLFormula
existsAlwaysParser = do
  spaces
  try (string "∃☐") <|> try (string "existsAlways")
  phi <- ctlParser
  return (ExistsAlways phi)

forAllNextParser :: CTLParser CTLFormula
forAllNextParser = do
  spaces
  try (string "∀X") <|> try (string "forAllNext")
  phi <- ctlParser
  return (ForAllNext phi)

forAllEventuallyParser :: CTLParser CTLFormula
forAllEventuallyParser = do
  spaces
  try (string "∀◇") <|> try (string "forAllEventually")
  phi <- ctlParser
  return (ForAllEventually phi)

forAllAlwaysParser :: CTLParser CTLFormula
forAllAlwaysParser = do
  spaces
  try (string "∀☐") <|> try (string "forAllAlways")
  phi <- ctlParser
  return (ForAllAlways phi)
