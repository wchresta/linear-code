{-# LANGUAGE ScopedTypeVariables, DataKinds #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
module Main where

import GHC.TypeLits (KnownNat, natVal)
import Data.Maybe (fromJust)
import Data.Proxy (Proxy(..))
import Control.Applicative (empty)

import Math.Algebra.Field.Base (eltsFq)
import Math.Code.Linear

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck
import Test.SmallCheck.Series

main :: IO ()
main = defaultMain tests

tests = testGroup "linear-code" [ fieldTests, codeTests ]

fieldTests :: TestTree
fieldTests = testGroup "Associativity"
    [ testProperty "Associativity for (F2,+)" $
        prop_associativity  ((+) :: F2 -> F2 -> F2)
    , testProperty "Associativity for (F2,*)" $
        prop_associativity  ((*) :: F2 -> F2 -> F2)
    ]

codeTests :: TestTree
codeTests =
    let tc = trivialCode :: BinaryCode 5 3
     in testGroup "Codes"
        [ testGroup "Trivial code"
            [ testCase "Trivial binary code == codeFromA zero, [5,3]" $
                tc @?= codeFromA (const 0)
            , testCase "Trivial binary code == codeFromA zero, [3,3]" $
                (trivialCode :: BinaryCode 3 3) @?= codeFromA (const 0)
            , testCase "Trivial binary code == codeFromA zero, [7,1]" $
                (trivialCode :: BinaryCode 7 1) @?= codeFromA (const 0)
            , testCase "zero vector is a code word" $
                assertBool ("H*c' = "++show (check tc mempty)) $
                    isCodeword tc mempty
            , testCase "ones-vector is not a code word" $
                let ones = fromJust $ cvector tc [1,1,1,1,1]
                 in assertBool ("H*c' = "++show (check tc ones)) $
                     not $ isCodeword tc ones
            ]
        , testGroup "Hamming(7,4)"
            [ testProperty "All generated codewords are codewords" $
                \((x,y,z,w)::(F2,F2,F2,F2)) -> isCodeword hamming74 
                                (fromJust $ codeword hamming74 [x,y,z,w])
            ]
        -- TODO: dualCode . dualCode == id
        --, testGroup "Code transformers"
        --    [ testProperty "Dual of dual is identitiy" $
        --        \(c :: LinearCode 7 4 F2) -> (dualCode . dualCode) c == c
        --    ]
        ]

-- SmallCheck Series for GF
instance forall m f. (Monad m, FiniteField f) => Serial m f where
        series = generate $ \d -> take (d+1) (eltsFq 1 :: [f])

{- QuickCheck Arbitrary
instance forall p s. (KnownNat p, KnownNat s) => Arbitrary (GF p s) where
    -- TODO: Generalise to s != 1
    arbitrary =
     let p' = fromInteger $ natVal (Proxy :: Proxy p)
     in do
        x <- choose (0,p')
        return . GF . toMod $ x
-}

prop_associativity :: Eq m => (m -> m -> m) -> m -> m -> m -> Bool
prop_associativity (%) x y z = (x % y) % z == x % (y % z)





