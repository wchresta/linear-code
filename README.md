# linear-code
Library to handle linear codes from coding theory.

The library is designed to carry the most important bits of information in the
type system while still keeping the types sane.

This library is based roughly on [/Introduction to Coding Theory/ by /Yehuda Lindell/](http://u.cs.biu.ac.il/~lindell/89-662/coding_theory-lecture-notes.pdf)

# Usage example
## Working with random codes
```Haskell
> :m + Math.Code.Linear System.Random
> :set -XDataKinds
> c <- randomIO :: IO (LinearCode 7 4 F5)
> c
[7,4]_5-Code
> generatorMatrix c
( 1 0 1 0 0 2 0 )
( 0 2 0 0 1 2 0 )
( 0 1 0 1 0 1 0 )
( 1 0 0 0 0 1 1 )
> e1 :: Vector 4 F5
( 1 0 0 0 )
> v = encode c e1
> v
( 1 0 1 0 0 2 0 )
> 2 ^* e4 :: Vector 7 F3
( 0 0 0 2 0 0 0 )
> vWithError = v + 2 ^* e4
> vWithError
( 1 0 1 2 0 2 0 )
> isCodeword c v
True
> isCodeword c vWithError
False
> decode c vWithError
Just ( 1 0 2 2 2 2 0 )
```
Notice, the returned vector is NOT the one without error. The reason for this
is that a random code most likely does not have a distance >2 which would be
needed to correct one error. Let's try with a hamming code

## Correcting errors with hamming codes
```Haskell
> c = hamming :: BinaryCode 7 4
> generatorMatrix c
( 1 1 0 1 0 0 0 )
( 1 0 1 0 1 0 0 )
( 0 1 1 0 0 1 0 )
( 1 1 1 0 0 0 1 )
> v = encode c e2
> vWithError = v + e3
> Just v' = decode c vWithError
> v' == v
True
```

