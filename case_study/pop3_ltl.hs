module Main where

import LTL
import TransitionSystem

k = Kripke
  {
    kripkeTS =
      [
        [False, True, False, False]
      , [False, False, True, False]
      , [False, False, False, True]
      , [True, False, False, False]
      ]
  , kripkeLabel =
      [
          (0, ["notConnected", "true","¬close", "¬transaction", "authorized^¬transaction"])
        , (1, ["authorization", "true","¬close", "¬close^open", "¬transaction", "authorized^¬transaction"])
        , (2, ["transaction", "true","¬close", "¬close^open"])
        , (3, ["update", "true", "¬transaction", "authorized^¬transaction"])
      ]
  }


{-
Buchi Automatons
-}
openImpliesEventuallyClosedBuchi = Buchi
  {
    buchiTS =
      [
          [Nothing, Just "¬close^open"]
        , [Nothing, Just "¬close"]
      ]
  , accepting =
      [
        1
      ]
  }

alwaysEventuallyTransactionBuchi = Buchi
  {
      buchiTS =
        [
            [Just "true", Just "¬transaction"]
          , [Nothing , Just "¬transaction"]
        ]
    , accepting =
      [
        1
      ]
  }

authImplXTransBuchi = Buchi
  {
      buchiTS =
        [
            [Nothing, Just "authorized", Nothing]
          , [Nothing, Nothing, Just "authorized^¬transaction"]
          , [Nothing, Nothing, Just "authorized"]
        ]
    , accepting =
      [
        1, 2, 3
      ]
  }

main :: IO()
main = do
  putStrLn "Open implies eventually closed"
  case eval0 of
    (True, prefix) -> putStrLn $ "Refuted:\npath: " ++ show prefix
    _ -> putStrLn "Property holds"
  putStrLn "Always eventually transaction"
  case eval1 of
    (True, prefix) -> putStrLn $ "Refuted:\npath: " ++ show prefix
    _ -> putStrLn "Property holds"
  putStrLn "Authorized implies next transaction"
  case eval1 of
    (True, prefix) -> putStrLn $ "Refuted:\npath: " ++ show prefix
    _ -> putStrLn "Property holds"
  where
    eval0 = evaluateLTL k openImpliesEventuallyClosedBuchi
    eval1 = evaluateLTL k alwaysEventuallyTransactionBuchi
    eval2 = evaluateLTL k authImplXTransBuchi
