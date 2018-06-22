{-# LANGUAGE ScopedTypeVariables, DataKinds #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
module Main where

import GHC.TypeLits (KnownNat, natVal)
import Numeric.Field.Finite
import Numeric.Code.Linear
import Data.Maybe (fromJust)
import Data.Proxy (Proxy(..))
import Data.Modular (toMod)
import Control.Applicative (empty)

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck
import Test.SmallCheck.Series

main :: IO ()
main = defaultMain tests

tests = testGroup "linear-code" [ fieldTests, codeTests ]

fieldTests :: TestTree
fieldTests = testGroup "Associativity"
    [ testProperty "Associativity for (GF2,+)" $
        prop_associativity  ((+) :: GF2 -> GF2 -> GF2)
    , testProperty "Associativity for (GF2,*)" $
        prop_associativity  ((*) :: GF2 -> GF2 -> GF2)
    ]

codeTests :: TestTree
codeTests =
    let tc = trivialBinaryCode :: BinaryCode 5 3
     in testGroup "Trivial code"
        [ testCase "Trivial binary code == codeFromA zero, [5,3]" $
            tc @?= codeFromA (const mempty)
        , testCase "Trivial binary code == codeFromA zero, [3,3]" $
            (trivialBinaryCode :: BinaryCode 3 3) @?= codeFromA (const mempty)
        , testCase "Trivial binary code == codeFromA zero, [7,1]" $
            (trivialBinaryCode :: BinaryCode 7 1) @?= codeFromA (const mempty)
        , testCase "zero vector is a code word" $
            assertBool ("H*c' = "++show (check tc mempty)) $
                isCodeword tc mempty
        , testCase "ones-vector is not a code word" $
            let ones = fromJust $ codeword tc [1,1,1,1,1]
             in assertBool ("H*c' = "++show (check tc ones)) $
                    isCodeword tc ones
        ]

-- SmallCheck Series for GF
instance forall m p s.
    (Monad m, KnownNat p, KnownNat s) => Serial m (GF p s) where
        series = GF . toMod <$> series `suchThat` (\x -> 0<=x && x<p'^s')
            where s' = fromIntegral . natVal $ (Proxy :: Proxy s)
                  p' = fromIntegral . natVal $ (Proxy :: Proxy p)


suchThat :: Series m a -> (a -> Bool) -> Series m a
suchThat s p = s >>= \x -> if p x then pure x else empty

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





