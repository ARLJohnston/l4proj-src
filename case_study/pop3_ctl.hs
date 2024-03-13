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

lookupTable :: [([Char], [Bool])]
lookupTable =
  [
     ("notConnected", [True, False, False, False, False, False, False])
   , ("authorization", [False, True, False, False, False, False, False])
   , ("transaction", [False, False, True, False, False, False, False])
   , ("list", [False, False, False, True, False, False, False])
   , ("retrieve", [False, False, False, False, True, False, False])
   , ("delete", [False, False, False, False, False, True, False])
   , ("update", [False, False, False, False, False, False, True])
   , ("authenticated", [False, False, True, True, True, True, True])
  ]

neverAuthenticated :: CTLFormula
neverAuthenticated = fromRight(CTLLabel []) $ runCTLParser "∃☐(¬((¬notConnected)^(¬authorization)))" lookupTable

notAuthenticatedUntilLogin :: CTLFormula
notAuthenticatedUntilLogin = fromRight (CTLLabel []) $ runCTLParser "∀(¬transaction)U(authenticated)" lookupTable

normalExecution :: CTLFormula
normalExecution = fromRight (CTLLabel []) $ runCTLParser "∃◇(notConnected ^ (∃Xauthorization ^ (∃Xtransaction ^ ∃Xupdate))))" lookupTable

allUsersEventuallyUpdate :: CTLFormula
allUsersEventuallyUpdate = fromRight(CTLLabel []) $ runCTLParser "∀◇update" lookupTable

main :: IO()
main = do
  putStrLn "Result of: Exists an execution which is never authenticated:"
  putStrLn $ show neverAuth ++ "\n"

  putStrLn "Result of: All paths are not authenticated until login:"
  putStrLn $ show notAuthUntilLogin ++ "\n"

  putStrLn "Result of: Exists some path which follows the 'normal' execution:"
  putStrLn $ show normalExec ++ "\n"

  putStrLn "Result of: All users eventuallly update:"
  putStrLn $ show allUsersExec ++ "\n"

  where
    normalExec = evaluateCTL normalExecution pop3Transitions
    neverAuth = evaluateCTL neverAuthenticated pop3Transitions
    notAuthUntilLogin = evaluateCTL notAuthenticatedUntilLogin pop3Transitions
    allUsersExec = evaluateCTL allUsersEventuallyUpdate pop3Transitions
