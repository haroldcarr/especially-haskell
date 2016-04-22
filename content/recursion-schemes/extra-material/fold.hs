------------------------------------------------------------------------------

foldr (+) 0 f1
-- => 15

foldl (+) 0 f1
-- => 15

foldr ((++) . show . head) "" f2
-- => "12345"

foldl ((++) . show . head) "" f2
-- =>     Couldn't match type ‘Int’ with ‘Char’

-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
foldr (\el acc -> show (head el) ++ acc) "" f2
-- => "12345"

-- foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
foldl (\acc el -> show (head el) ++ acc) "" f2
-- => "54321"

foldl (\acc el -> acc ++ show (head el)) "" f2
-- => "12345"

-- challenge : write point-free version of last expression

------------------------------------------------------------------------------

:{
let f3 = B (B (B (L  (Sum 1))   (Sum 2) E)
        (Sum 3)
        (L  (Sum 4)))
     (Sum 5)
     E
:}

fold f3

import Data.Monoid

:{
let f4 = B (B (B (L  (Product 1))   (Product 2) E)
        (Product 3)
        (L  (Product 4)))
     (Product 5)
     E
:}

fold f4

------------------------------------------------------------------------------

foldr1 (+) f1

import Data.Foldable

toList f1
toList f2

sum f1
product f1

------------------------------------------------------------------------------

     zero
outF zero
inF (outF zero)
outF (Fix ZeroF)
inF  N :: List Int
inF  N :: [Int]
outF []
      cons 1 (cons 2 nil)
outF (cons 1 (cons 2 nil))
outF [1,2]


------------------------------------------------------------------------------

:{
let lengthX = foldrP alg where
    alg :: Maybe (a, Int) -> Int
    alg Nothing        = 0
    alg (Just (_, xs)) = xs + 1
:}

:hog unfoldr
Data.List unfoldr :: (b -> Maybe (a, b)) -> b -> [a]


