# Hashids

This is a direct translation of [vinkla/hashids](https://github.com/vinkla/hashids).
It is implemented using the ST monad in order to make it as similar as possible to the php implementation.
The test suite from `vinkla/hashids` has been translated to hspec to ensure complete compatability.

## Usage

```haskell
doEncoding =
  let Right ctx = mkHashidsContext "this is my salt" 8 defaultAlphabet
  encode ctx [1]
```
