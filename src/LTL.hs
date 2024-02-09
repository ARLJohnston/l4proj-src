module LTL (module LTL) where

import Data.List (intercalate)

--https://book.realworldhaskell.org/read/using-typeclasses.html
-- https://hackage.haskell.org/package/base-4.19.0.0/docs/src/Data.Foldable.html#Foldable

--class SinglyLinkedList l where
--    insert :: a -> l a -> l a
--    isEmpty :: l a -> Bool
--    post :: (Eq a) => l a -> a -> l a
--
--instance SinglyLinkedList [] where
--    insert x xs = x:xs
--    isEmpty [] = True
--    isEmpty _  = False
--    post [] _ = []
--    post (x:xs) elem = if x == elem then xs else post xs elem
--
---- Define the instance for the SinglyLinkedList typeclass
--data Node a = Node a (Node a) | Empty deriving Show
--
--instance SinglyLinkedList Node where
--    insert x xs = Node x xs
--
--
--    isEmpty Empty = True
--    isEmpty _     = False
--
--    post Empty _ = Empty
--    post (Node x xs) elem = if x == elem then xs else post xs elem

--class LTL l where
--  pre :: l a -> Int -> Maybe a
--  post :: l a -> Int -> Maybe a
--  getElement :: l a -> Int -> Maybe a
--
--instance LTL [] where
--  pre [] _ = Nothing
--  pre l n = getElement l (n-1)
--
--  post [] _ = Nothing
--  post l n = getElement l (n+1)
--
--  getElement [] _ = Nothing
--  getElement l n
--    | n>=0 && n < length l = Just (l !! n)
--    | otherwise = Nothing
  --getElement (x:xs) elem = if x == elem then xs else getElement xs elem

-- https://www.fpcomplete.com/blog/tying-the-knot-haskell/
data LTLStructure a = LTLStructure 
  {
    -- prev  :: Maybe (Node a)
     nodeID :: Int
  ,  value :: a
  ,  next  :: LTLStructure a
  }

--data ListWithHistory a = ListWithHistory [a] [a]
--
--instance (Show a) => Show (ListWithHistory a) where
--    show (ListWithHistory visited current) = "ListWithHistory " ++ show visited ++ " " ++ show current
--
--main :: IO ()
--main = do
--    let myList = ListWithHistory [] [1, 2, 3, 4, 5]
--    putStrLn $ show myList

--getInfiniteList :: Eq a => [a] -> [a] -> [a]
--getInfiniteList [] _ = []
--getInfiniteList (x:xs) accumulator
--  | x `elem` accumulator = accumulator ++ [x]
--  | otherwise = getInfiniteList xs (accumulator ++ [x])

getInfiniteList :: Eq a => LTLStructure a -> [LTLStructure a] -> [LTLStructure a]
getInfiniteList l accumulator
  | l `elem` accumulator = accumulator ++ [l]
  | otherwise = getInfiniteList (next l) (accumulator ++ [l])

getValue :: LTLStructure a -> a
getValue l = value l

instance (Show a, Eq a) => Show (LTLStructure a) where
   show l = show $ intercalate "->" $ map (show . getValue) $ getInfiniteList l [] 

instance Eq a => Eq (LTLStructure a) where
  -- Each Node in an LTL is required to have precisely one successor
  (==) a b = nodeID a == nodeID b

data LTLFormula =
    LTLLabel [Bool]
  | LTLAtom [Bool]
  | LTLAnd LTLFormula LTLFormula
  | LTLNot LTLFormula
  | Next LTLFormula
  | Until LTLFormula LTLFormula
    deriving (Eq)

instance Show LTLFormula where
  show (LTLLabel satisfy) = "Sat(" ++ show satisfy ++ ")"
  show (LTLAtom satisfy) = "Sat(" ++ show satisfy ++ ")"
  show (LTLAnd phi psi) = "(" ++ show phi ++ ") ^ (" ++ show psi ++ ")"
  show (LTLNot phi) = "¬(" ++ show phi ++ ")"
  show (Next phi) = "X(" ++ show phi ++ ")"
  show (Until phi psi) = "(" ++ show phi ++ ") U (" ++ show psi ++ ")"
