module Main where

import CTL
import CTLParser

import Data.Either (fromRight)

pop3Transitions :: [[Bool]]
pop3Transitions =
  [
    [False, True, False, False, False, False, False], -- Not connected
    [False, True, True, False, False, False, False], -- Authorization
    [False, False, True, True, True, True, True], -- Transaction
    [False, False, True, False, False, False, False], -- List
    [False, False, True, False, False, False, False], -- Retrieve
    [False, False, True, False, False, False, False], -- Delete
    [True, False, False, False, False, False, False]  -- Update
  ]

{-
Want to prove:
Exists an execution which is never authenticated
  ∃☐(¬auth)

All paths are not authenticated until login
  ∀(¬auth)U(login)

Exists some path which follows the 'normal' exection
  ∃(notConnected U (auth U (transaction U (update))))

-}

lookupTable :: [([Char], [Bool])]
lookupTable =
  [
      ("auth", [False, False, True, True, True, True, True])
    , ("login", [False, False, True, True, True, True, True])
    , ("notConnected", [True, True, False, False, False, False, False])
    , ("update", [False, False, False, False, False, False, True])
    , ("transaction", [False, False, True, False, False, False, False])
  ]

neverAuthenticated :: CTLFormula
neverAuthenticated = fromRight (CTLLabel []) $ runCTLParser "∃☐(¬auth)" lookupTable

notAuthenticatedUntilLogin :: CTLFormula
notAuthenticatedUntilLogin = fromRight (CTLLabel []) $ runCTLParser "∀(¬auth)U(login)" lookupTable

normalExecution :: CTLFormula
normalExecution = fromRight (CTLLabel []) $ runCTLParser "notConnected ^ (∃Xauth ^ (∃Xtransaction ^ (∃X update ^ ∃XnotConnected)))" lookupTable

main :: IO()
main = do
  putStrLn "Result of: Exists an execution which is never authenticated: ∃☐(¬auth)"
  putStrLn $ show neverAuth

  putStrLn "Result of: All paths are not authenticated until login: ∀(¬auth)U(login)"
  putStrLn $ show notAuthUntilLogin

  putStrLn "Result of: Exists some path which follows the 'normal' exection: ∃(notConnected U (auth U (transaction U (update))))"
  putStrLn $ show normalExec 

  where
    neverAuth = evaluateCTL neverAuthenticated pop3Transitions
    notAuthUntilLogin = evaluateCTL notAuthenticatedUntilLogin pop3Transitions
    normalExec = evaluateCTL normalExecution pop3Transitions

  --print hello world


-- main :: IO ()
-- main = do
--   putStrLn "Hello, Haskell!"
