0.2.0
-----
* Major changes
  - Replaced matrix with matrix-static
  - Removed Data.Algebra.Matrix
  - No reexporting of Matrix functions anymore

* Minor changes
  - Fixed base min version to 4.9
  - Fixed some static equations to allow support for GHC<8.4
  - Default stackage lts resolver is lts-12.2


0.1.1
-----
* Backwards compatible changes
  - Add golay code
  - Add `codeFromAD`, `dualCodeD` creators
  - Add `extendCode` code transformer
  - Replace `combinat` dependency with `random-shuffle`

* Bugfixes
  - calcSyndromeTable uses known code distances correctly


0.1.0
-----

* Initial release
  - Includes trivial, hamming and random codes
  - Implements syndrome decoding

