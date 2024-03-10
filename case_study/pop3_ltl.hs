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
          (0, ['n', 'z'])
        , (1, ['a', 'z'])
        , (2, ['t', 'z'])
        , (3, ['u', 'z'])
      ]
  }


{-
Buchi Automatons
-}
openImpliesEventuallyClosedBuchi = Buchi
  {
    buchiTS =
      [
          [Nothing, Just 'z', Just 'u']
        , [Nothing, Just 'z', Just 'u']
        , [Nothing, Nothing, Just 'z']
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
            [Just 'z', Just 't']
          , [Just 'z', Just 't']
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
            [Just 'z', Just 'a']
          , [Just 'z', Just 't']
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
