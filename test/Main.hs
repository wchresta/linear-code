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
                tc @?= codeFromA zero
            , testCase "Trivial binary code == codeFromA zero, [3,3]" $
                (trivialCode :: BinaryCode 3 3) @?= codeFromA zero
            , testCase "Trivial binary code == codeFromA zero, [7,1]" $
                (trivialCode :: BinaryCode 7 1) @?= codeFromA zero
            , testCase "zero vector is a code word" $
                assertBool ("H*c' = "++show (check tc zero)) $
                    isCodeword tc zero
            , testCase "ones-vector is not a code word" $
                let ones = fromList [1,1,1,1,1]
                 in assertBool ("H*c' = "++show (check tc ones)) $
                     not $ isCodeword tc ones
            ]
        , testGroup "Random Code"
            [ testProperty "All generated codewords are codewords" $
                \((c,x,y,z,w)::(BinaryCode 7 4,F2,F2,F2,F2)) 
                    -> isCodeword c (codeword c (fromList [x,y,z,w]))
            ]

        , testGroup "Hamming(7,4)"
            [ testProperty "All generated codewords are codewords" $
                \((x,y,z,w)::(F2,F2,F2,F2)) -> isCodeword hamming74 
                                (codeword hamming74 (fromList [x,y,z,w]))
            ]
        --, testGroup "Code transformers"
        --    [ testProperty "Dual of dual is identitiy" $
        --        \(c :: LinearCode 7 4 F2) -> (dualCode . dualCode) c == c
        --    ]
        ]

-- SmallCheck Series for GF
instance forall m f. (Monad m, FiniteField f) => Serial m f where
    series = generate $ \d -> take (d+1) (eltsFq 1 :: [f])

-- Smallcheck for LinearCodes
{- -- TODO: Add Serial for LinearCodes
instance forall n k f. 
    (KnownNat n, KnownNat k, FiniteField f) => Serial (LinearCode n k) f where
        series = generate $ \n -> 
            let consNKN f = decDepth $ f
                k = n `div` 2
                as = consNKN
             in codeFromA <$> as
-}

prop_associativity :: Eq m => (m -> m -> m) -> m -> m -> m -> Bool
prop_associativity (%) x y z = (x % y) % z == x % (y % z)

