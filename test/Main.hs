{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeOperators, TypeFamilies #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import GHC.TypeLits (KnownNat, natVal, type (<=))
import Data.Proxy (Proxy(..))

import qualified Math.Algebra.Matrix as M
import Math.Algebra.Field.Instances() -- Import random instances
import qualified Math.Core.Utils as F
import qualified Math.Algebra.Field.Base as F
import qualified Math.Algebra.Field.Extension as F
import qualified Math.Common.IntegerAsType as F
import Math.Algebra.Code.Linear
import System.Random (Random)

import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.SmallCheck as S
import qualified Test.Tasty.QuickCheck as Q
import qualified Test.SmallCheck.Series as S

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "linear-code" [ fieldTests, codeTests ]

fieldTests :: TestTree
fieldTests = testGroup "Associativity"
    [ S.testProperty "Associativity for (F2,+)" $
        prop_associativity  ((+) :: F2 -> F2 -> F2)
    , S.testProperty "Associativity for (F2,*)" $
        prop_associativity  ((*) :: F2 -> F2 -> F2)
    ]

codeTests :: TestTree
codeTests =
    let tc = trivialCode :: BinaryCode 5 3
        hamming74 = hamming :: BinaryCode 7 4
        eHamming94 = extendCode hamming74 :: BinaryCode 9 4
     in testGroup "Codes"
        [ testGroup "Instances"
            [ testCase "Show works for unknown distance" $
                show (trivialCode {distance=Nothing} :: LinearCode 7 4 F.F3)
                    @?= "[7,4]_3-Code"
            , testCase "Show works for known distance" $
                show (trivialCode {distance=Just 3} :: LinearCode 7 4 F.F3)
                    @?= "[7,4,3]_3-Code"
            ]
        , testGroup "Trivial code"
            [ testCase "Trivial binary code == codeFromA zero, [5,3]" $
                tc @?= codeFromA zero
            , testCase "Trivial binary code == codeFromA zero, [3,3]" $
                (trivialCode :: BinaryCode 3 3) @?= codeFromA zero
            , testCase "Trivial binary code == codeFromA zero, [7,1]" $
                (trivialCode :: BinaryCode 7 1) @?= codeFromA zero
            , testCase "zero vector is a code word" $
                assertBool ("H*c' = "++show (syndrome tc zero)) $
                    isCodeword tc zero
            , testCase "ones-vector is not a code word" $
                let ones = fromList [1,1,1,1,1]
                 in assertBool ("H*c' = "++show (syndrome tc ones)) $
                     not $ isCodeword tc ones
            ]
        , testGroup "Random Code"
            [ Q.testProperty "Random code generation works" $
                \(c :: LinearCode 7 4 F.F3) -> seq c True
            , Q.testProperty "All generated codewords are codewords" $
                \c x y z w -> isCodeword (c :: LinearCode 7 4 F.F5) $
                    encode c $ fromList ([x,y,z,w] :: [F.F5])
            ]
        , testGroup "Hamming(7,4)"
            [ S.testProperty "All encoded words are codewords" $
                \((x,y,z,w)::(F2,F2,F2,F2)) -> isCodeword hamming74
                                (encode hamming74 (fromList [x,y,z,w]))
            , Q.testProperty "List all codewords" $
                \(c :: LinearCode 7 4 F.F5) ->
                    length (codewords c) == 5^(4 :: Int)
            , Q.testProperty "Simple decode of single error" $
                \(v :: Vector 4 F2) ->
                    let w = encode hamming74 v :: Vector 7 F2
                     in decode hamming74 (w + e2) == Just w
            ]
        , testGroup "Code transformers"
            [ Q.testProperty "dualCode . dualCode == id" $
                \(c :: LinearCode 9 3 F.F4) ->
                    c == (dualCode . dualCode $ c)
            ]
{- This test is too slow
   , testGroup "Golay"
            [ testCase "Golay can correct 3 errors" $
                -- \((w,a,b,c) :: (Vector 12 F2,F2,F2,F2)) ->
                let w = fromList [0,0,1,0,1,1,1,1,0,1,0,1] :: Vector 12 F2
                    (a,b,c) = (1,1,1) :: (F2,F2,F2)
                 in
                    let v = encode golay w
                        ve = v + a M.^* e3 + b M.^* e7 + c M.^* eVec 14
                     in decode golay ve @?= Just v
            ]
-}
        , testGroup "Standard form"
            [ Q.testProperty "Standard form of standard form is equal" $
                \(c :: LinearCode 7 4 F.F3) ->
                    let sc = standardFormGenerator c
                     in sc == standardForm sc
            ]
        , testGroup "Code transformers"
            [ Q.testProperty "Dual of dual is identitiy" $
                \(c :: LinearCode 7 4 F2) -> (dualCode . dualCode) c == c
            , Q.testProperty "Extended codes are of same distance" $
                \(c :: LinearCode 7 4 F5) ->
                    distance (extendCode c :: LinearCode 9 4 F5) == distance c
            , testCase "Extended hamming have distance 3" $
                distance (extendCode hamming74 :: BinaryCode 9 4) @?= Just 3
{- -- These tests are too slow
            , Q.testProperty "Extended hamming can correct 1 error" $
                \(v :: Vector 4 F2) ->
                    let w = encode eHamming94 v
                     in decode eHamming94 (w + e3) == Just w
            , Q.testProperty "Extended hamming can correct 1 in extension" $
                \(v :: Vector 4 F2) ->
                    let w = encode eHamming94 v
                     in decode eHamming94 (w + e8) == Just w
-}
            ]
        ]

-- SmallCheck Series for GF
instance forall m f. (Monad m, F.FiniteField f) => S.Serial m f where
    series = S.generate $ \d -> take (d+1) (F.eltsFq 1 :: [f])

instance forall m n f. (KnownNat m, KnownNat n, Q.Arbitrary f)
  => Q.Arbitrary (M.Matrix m n f) where
    arbitrary = fromList <$> Q.vectorOf (n*m) Q.arbitrary
      where
        n = fromInteger . natVal $ (Proxy :: Proxy n)
        m = fromInteger . natVal $ (Proxy :: Proxy m)

instance forall p. F.IntegerAsType p => Q.Arbitrary (F.Fp p) where
    arbitrary = Q.arbitraryBoundedRandom

instance forall n k f.
    (KnownNat n, KnownNat k, k <= n, Num f, Ord f, Eq f, F.FinSet f, Random f)
  => Q.Arbitrary (LinearCode n k f) where
    arbitrary = Q.arbitraryBoundedRandom


prop_associativity :: Eq m => (m -> m -> m) -> m -> m -> m -> Bool
prop_associativity (%) x y z = (x % y) % z == x % (y % z)

-- vim : set colorcolumn=80
