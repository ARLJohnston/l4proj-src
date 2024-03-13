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
          (0, ["notConnected", "¬close", "¬transaction"])
        , (1, ["authorization", "¬close", "¬close^open", "¬transaction", "authorized^¬transaction"])
        , (2, ["transaction", "¬close", "¬close^open"])
        , (3, ["update", "¬transaction"])
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
            [Nothing , Just "¬transaction"]
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

lect16Kripke = Kripke {
    kripkeTS =
      [
        [False, True, True]
      , [False, True, False]
      , [False, False, True]
      ]
  , kripkeLabel =
      [
          (0, ["¬a"])
        , (1, ["¬a"])
        , (2, ["a"])
      ]
}

lect16Buchi = Buchi
  {
      buchiTS =
        [
            [Nothing, Just "a", Nothing]
          , [Nothing, Just "a", Just "¬a"]
          , [Nothing, Nothing, Nothing]
        ]
    , accepting =
      [
        1
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
  case eval2 of
    (True, prefix) -> putStrLn $ "Refuted:\npath: " ++ show prefix
    _ -> putStrLn "Property holds"
  where
    eval0 = evaluateLTL k openImpliesEventuallyClosedBuchi
    eval1 = evaluateLTL k alwaysEventuallyTransactionBuchi
    eval2 = evaluateLTL k authImplXTransBuchi
