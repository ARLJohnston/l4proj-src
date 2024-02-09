module CTLParser (runCTLParser) where

import Text.Parsec
import CTL

type CTLParser a = Parsec String [([Char], [Bool])] a

-- | Generates a 'CTLFormula' given a formula and satisfaction sets.
--
-- > lookupTable :: [([Char], [Bool])]
-- > lookupTable =
-- >   [
-- >       ("satA", [False, False])
-- >   ]
-- >
-- > parseResult = runCTLParser "¬satA" lookupTable = Right ¬(Sat([False, False]))
runCTLParser :: String -> [([Char], [Bool])] -> Either ParseError CTLFormula
runCTLParser input lookupTable = runParser ctlParser lookupTable "CTL Parser" input

getKeys :: [([Char], a)] -> [[Char]]
getKeys [] = []
getKeys ((s, _):xs) = s : getKeys xs

-- Main Parser
ctlParser :: CTLParser CTLFormula
ctlParser = do
  phi <- start
  maybePsi <- end ["^"]
  case maybePsi of
    Nothing -> return phi
    Just psi -> return (CTLAnd phi psi)

-- Non-Recursive parser
satisfactionParser :: CTLParser CTLFormula
satisfactionParser = do
  lookupTable <- getState
  spaces
  value <- choice $ map (try . string) $ getKeys lookupTable
  case lookup value lookupTable of
    Just sat -> return $ CTLLabel sat
    Nothing -> fail "Unable to match any given SAT values"

-- Start parsers
start :: CTLParser CTLFormula
start =
      satisfactionParser
  <|> groupParser
  <|> notParser
  <|> existsNextParser
  <|> existsAlwaysParser
  <|> existsEventuallyParser
  <|> forAllNextParser
  <|> forAllEventuallyParser
  <|> forAllAlwaysParser
  <|> untilParser

end :: [[Char]] -> CTLParser (Maybe CTLFormula)
end op =
      getEnd
  <|> return Nothing
  where
    getEnd = do
      spaces
      choice $ map (try . string) op
      spaces
      Just <$> ctlParser

untilParser :: CTLParser CTLFormula
untilParser = do
  expr1 <- startUntil
  maybeExpr2 <- end ["U", "until"]
  case maybeExpr2 of
    Nothing -> error "No psi specified for until clause"
    Just psi -> case expr1 of
      ForAllPhiUntilPsi phi _ -> return (ForAllPhiUntilPsi phi psi)
      ExistsPhiUntilPsi phi _ -> return (ExistsPhiUntilPsi phi psi)

startUntil :: CTLParser CTLFormula
startUntil =
      forAllParser
  <|> existsParser

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
  choice $ map (try . string) ["∀", "forAll", "A"]
  phi <- ctlParser
  return (ForAllPhiUntilPsi phi phi)

existsParser :: CTLParser CTLFormula
existsParser = do
  spaces
  choice $ map (try . string) ["∃", "exists", "E"]
  phi <- ctlParser
  return (ExistsPhiUntilPsi phi phi)

notParser :: CTLParser CTLFormula
notParser = do
  spaces
  choice $ map (try . string) ["¬", "not", "!"]
  CTLNot <$> ctlParser

existsNextParser :: CTLParser CTLFormula
existsNextParser = do
  spaces
  choice $ map (try . string) ["∃X", "existsNext", "EX"]
  ExistsNext <$> ctlParser

existsAlwaysParser :: CTLParser CTLFormula
existsAlwaysParser = do
  spaces
  choice $ map (try . string) ["∃☐", "existsAlways", "EG"]
  ExistsAlways <$> ctlParser

existsEventuallyParser :: CTLParser CTLFormula
existsEventuallyParser = do
  spaces
  choice $ map (try . string) ["∃◇", "existsEventually", "EF"]
  ExistsEventually <$> ctlParser

forAllNextParser :: CTLParser CTLFormula
forAllNextParser = do
  spaces
  choice $ map (try . string) ["∀X", "forAllNext", "AX"]
  ForAllNext <$> ctlParser

forAllEventuallyParser :: CTLParser CTLFormula
forAllEventuallyParser = do
  spaces
  choice $ map (try . string) ["∀◇", "forAllEventually", "AF"]
  ForAllEventually <$> ctlParser

forAllAlwaysParser :: CTLParser CTLFormula
forAllAlwaysParser = do
  spaces
  choice $ map (try . string) ["∀☐", "forAllAlways", "AG"]
  ForAllAlways <$> ctlParser
