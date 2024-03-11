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
          (0, ["n", "true"])
        , (1, ["a", "true"])
        , (2, ["t", "true"])
        , (3, ["u", "true"])
      ]
  }


{-
Buchi Automatons
-}
openImpliesEventuallyClosedBuchi = Buchi
  {
    buchiTS =
      [
          [Nothing, Just "true", Just "u"]
        , [Nothing, Just "true" , Just "u"]
        , [Nothing, Nothing, Just "true"]
      ]
  , accepting =
      [
        2
      ]
  }

alwaysEventuallyTransactionBuchi = Buchi
  {
      buchiTS =
        [
            [Just "true", Just "t"]
          , [Just "true", Just "t"]
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
            [Just "true", Just "a"]
          , [Just "true", Just "t"]
        ]
    , accepting =
      [
        1
      ]
  }

main :: IO()
main = do
  case eval0 of
    (True, prefix) -> putStrLn $ "Refuted:\npath: " ++ show prefix
    _ -> putStrLn "Property holds"
  where
    eval0 = evaluateLTL k openImpliesEventuallyClosedBuchi
