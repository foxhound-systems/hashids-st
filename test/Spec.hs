{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import Test.QuickCheck
import Hashids
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Char (toLower)
import Data.Maybe (listToMaybe)
import Text.Read (readMaybe)
import Control.Applicative

testSmallAlphabet =
  it "throws an exception for a small alphabet" $ 
    let context = mkHashidsContext "" 0 "1234567890"
    in context `shouldBe` (Left AlphabetToSmall)

testAlphabetWithSpaces =
  it "throws an exception for an alphabet with spaces" $ 
    let context = mkHashidsContext "" 0 "a cdefghijklmnnopqrstuvwxyz"
    in context `shouldBe` (Left AlphabetContainsSpaces)

testBadInput =
  it "should handle bad input gracefully" $ do
    let Right ctx = mkHashidsContext "" 0 defaultAlphabet
    encode ctx [] `shouldBe` ""
    encode ctx [-1] `shouldBe` ""
    decode ctx "" `shouldBe` []
    decode ctx "f" `shouldBe` [] 

    encodeHex ctx "z" `shouldBe` ""
    decodeHex ctx "f" `shouldBe` []

defaultParamsHexData = 
  [ ("wpVL4j9g", "deadbeef")
  , ("kmP69lB3xv", "abcdef123456")
  , ("47JWg0kv4VU0G2KBO2", "ABCDDD6666DDEEEEEEEEE")
  , ("y42LW46J9luq3Xq9XMly", "507f1f77bcf86cd799439011")
  , ("m1rO8xBQNquXmLvmO65BUO9KQmj", "f00000fddddddeeeee4444444ababab")
  , ("wBlnMA23NLIQDgw7XxErc2mlNyAjpw", "abcdef123456abcdef123456abcdef123456")
  , ("VwLAoD9BqlT7xn4ZnBXJFmGZ51ZqrBhqrymEyvYLIP199", "f000000000000000000000000000000000000000000000000000f")
  , ("nBrz1rYyV0C0XKNXxB54fWN0yNvVjlip7127Jo3ri0Pqw", "fffffffffffffffffffffffffffffffffffffffffffffffffffff")
  ]

testDefaultParamsHex =
  it "encodes hex" $ do 
    forM_ defaultParamsHexData $ \(hash, hex) -> do
      let Right ctx = mkHashidsContext "" 0 defaultAlphabet
      let encodedHash = encodeHex ctx hex
          decodedHex = decodeHex ctx encodedHash
      encodedHash `shouldBe` hash
      fmap toLower hex `shouldBe` decodedHex

 
alphabets =
        [
            "cCsSfFhHuUiItT01",
            "abdegjklCFHISTUc",
            "abdegjklmnopqrSF",
            "abdegjklmnopqrvwxyzABDEGJKLMNOPQRVWXYZ1234567890",
            "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890`~!@#$%^&*()-_=+\\|\"\";:/?.>,<{[}]",
            "`~!@#$%^&*()-_=+\\|\"\";:/?.>,<{[}]",
            "áàãăâeéèêiíìĩoóòõôơuúùũưyýỳđ"
        ]

testAlphabet =
  it "can encode with custom alphabets" $ do
    forM_ alphabets $ \alphabet -> do
      let numbers = [1, 2, 3]
          Right ctx = mkHashidsContext "" 0 alphabet
          hash = encode ctx numbers
      decode ctx hash `shouldBe` numbers


salts =
  [ ""
  , "0"
  , "   "
  , "this is my salt"
  , "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890`~!@#$%^&*()-_=+\\|'\";:/?.>,<{[}]"
  , "`~!@#$%^&*()-_=+\\|'\";:/?.>,<{[}]"
  , "!áàãăâ eéèê iíìĩ oóòõôơ uúùũư yýỳ đ"
  ]

testSalt =
  it "Should take custom salts" $ do
    let numbers = [1, 2, 3]
    forM_ salts $ \salt -> do
      let Right ctx = mkHashidsContext salt 0 defaultAlphabet
          hashedValue = encode ctx numbers
      decode ctx hashedValue `shouldBe` numbers

testMinLength = 
  it "Can take a custom minLength" $ do
    let numbers = [1,2,3]
        minLengths = [ 0, 1, 10, 999, 1000 ]
    forM_ minLengths $ \minLength -> do
      let Right ctx = mkHashidsContext "" minLength defaultAlphabet
          hashedValue = encode ctx numbers
      decode ctx hashedValue `shouldBe` numbers
      hashedValue `shouldSatisfy` ((>= minLength) . toInteger . length)

defaultParams =
    [ ("gY", [0])
    , ("jR", [1])
    , ("R8ZN0", [928728])
    , ("o2fXhV", [1, 2, 3])
    , ("jRfMcP", [1, 0, 0])
    , ("jQcMcW", [0, 0, 1])
    , ("gYcxcr", [0, 0, 0])
    , ("gLpmopgO6", [1000000000000])
    , ("lEW77X7g527", [9007199254740991])
    , ("BrtltWt2tyt1tvt7tJt2t1tD", [5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5])
    , ("G6XOnGQgIpcVcXcqZ4B8Q8B9y", [10000000000, 0, 0, 0, 999999999999999])
    , ("5KoLLVL49RLhYkppOplM6piwWNNANny8N", [9007199254740991, 9007199254740991, 9007199254740991])
    , ("BPg3Qx5f8VrvQkS16wpmwIgj9Q4Jsr93gqx", [1000000001, 1000000002, 1000000003, 1000000004, 1000000005])
    , ("1wfphpilsMtNumCRFRHXIDSqT2UPcWf1hZi3s7tN", [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20])
    ]
testDefaultParams =
  it "works with the default params" $ do
    let Right ctx = mkHashidsContext "" 0 defaultAlphabet
    forM_ defaultParams $ \(hash, numbers) -> do
      let encodedHash = encode ctx numbers
          decodedNumbers = decode ctx encodedHash

      encodedHash `shouldBe` hash
      decodedNumbers `shouldBe` numbers

customParams =
  [ ("nej1m3d5a6yn875e7gr9kbwpqol02q", [0])
  , ("dw1nqdp92yrajvl9v6k3gl5mb0o8ea", [1])
  , ("onqr0bk58p642wldq14djmw21ygl39", [928728])
  , ("18apy3wlqkjvd5h1id7mn5ore2d06b", [1, 2, 3])
  , ("o60edky1ng3vl9hbfavwr5pa2q8mb9", [1, 0, 0])
  , ("o60edky1ng3vlqfbfp4wr5pa2q8mb9", [0, 0, 1])
  , ("qek2a08gpl575efrfd7yomj9dwbr63", [0, 0, 0])
  , ("m3d5a6yn875rae8y81a94gr9kbwpqo", [1000000000000])
  , ("1q3y98ln48w96kpo0wgk314w5mak2d", [9007199254740991])
  , ("op7qrcdc3cgc2c0cbcrcoc5clce4d6", [5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5])
  , ("5430bd2jo0lxyfkfjfyojej5adqdy4", [10000000000, 0, 0, 0, 999999999999999])
  , ("aa5kow86ano1pt3e1aqm239awkt9pk380w9l3q6", [9007199254740991, 9007199254740991, 9007199254740991])
  , ("mmmykr5nuaabgwnohmml6dakt00jmo3ainnpy2mk", [1000000001, 1000000002, 1000000003, 1000000004, 1000000005])
  , ("w1hwinuwt1cbs6xwzafmhdinuotpcosrxaz0fahl", [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20])
  ]

testCustomParams = 
  it "can handle custom params" $ do
    let minLength = 30
        Right ctx = mkHashidsContext "this is my salt" minLength "xzal86grmb4jhysfoqp3we7291kuct5iv0nd"
    forM_ customParams $ \(hash, numbers) -> do
      let encodedHash = encode ctx numbers
          decodedNumbers = decode ctx encodedHash
      encodedHash `shouldBe` hash
      decodedNumbers `shouldBe` numbers
      encodedHash `shouldSatisfy` ((>= minLength) . toInteger . length)

customParamsHex = 
  [ ("0dbq3jwa8p4b3gk6gb8bv21goerm96", "deadbeef")
  , ("190obdnk4j02pajjdande7aqj628mr", "abcdef123456")
  , ("a1nvl5d9m3yo8pj1fqag8p9pqw4dyl", "ABCDDD6666DDEEEEEEEEE")
  , ("1nvlml93k3066oas3l9lr1wn1k67dy", "507f1f77bcf86cd799439011")
  , ("mgyband33ye3c6jj16yq1jayh6krqjbo", "f00000fddddddeeeee4444444ababab")
  , ("9mnwgllqg1q2tdo63yya35a9ukgl6bbn6qn8", "abcdef123456abcdef123456abcdef123456")
  , ("edjrkn9m6o69s0ewnq5lqanqsmk6loayorlohwd963r53e63xmml29", "f000000000000000000000000000000000000000000000000000f")
  , ("grekpy53r2pjxwyjkl9aw0k3t5la1b8d5r1ex9bgeqmy93eata0eq0", "fffffffffffffffffffffffffffffffffffffffffffffffffffff")
  ]

testCustomParamsHex =
  it "can handle hex with custom params" $ do
    let minLength = 30
        Right ctx = mkHashidsContext "this is my salt" minLength "xzal86grmb4jhysfoqp3we7291kuct5iv0nd"
    forM_ customParamsHex $ \(hash, hex) -> do
      let encodedHash = encodeHex ctx hex
          decodedHex = decodeHex ctx encodedHash 
      encodedHash `shouldBe` hash
      fmap toLower hex `shouldBe` decodedHex
      encodedHash `shouldSatisfy` ((>= minLength) . toInteger . length)



bigNumberData :: [(Integer, String)]
bigNumberData =
    [ (2147483647, "ykJWW1g") --max 32-bit signed integer
    , (4294967295, "j4r6j8Y") -- max 32-bit unsigned integer
    , (9223372036854775807, "jvNx4BjM5KYjv") -- max 64-bit signed integer
    , (18446744073709551615, "zXVjmzBamYlqX") -- max 64-bit unsigned integer
    ]

testBigNumberEncode =
  it "can encode big numbers" $ do
    forM_ bigNumberData $ \(number, hash) -> do
      let Right ctx = mkHashidsContext "this is my salt" 0 defaultAlphabet
          encoded = encode ctx [number]
      encoded `shouldBe` hash

testBigNumberDecode =
  it "can encode big numbers" $ do
    forM_ bigNumberData $ \(number, hash) -> do
      let Right ctx = mkHashidsContext "this is my salt" 0 defaultAlphabet
          decoded = decode ctx hash
      decoded `shouldBe` [number]

jsHashidsCompatibleExamples = 
  [ ("", 0, "áàãăâeéèêiíìĩoóòõôơuúùũưyýỳđ", [1, 2, 3, 4, 5, 6, 7, 8, 9, 10], "íóuđáìàúãỳăyâôeyiôuĩ")
  , ("世界", 0, "áàãăâeéèêiíìĩoóòõôơuúùũưyýỳđ", [9007199254740991], "óôòúỳưúoỳééưýy")
  , ("", 0, "cCsSfFhHuUiItT01", [1, 2, 3, 4, 5, 6, 7, 8, 9, 10], "10h10i00s100t010u110C000F1000H0110I1010")
  , ("", 9, "零0一1二2三3四4五5六6七7七8八9九", [4231], "5三77九58三九")
  ]

testJsHashidsCompatibleExamples =
  it "is compatible with js" $ do
    forM_ jsHashidsCompatibleExamples $ \(salt, minLen, alphabet, input, output) -> do
      let Right ctx = mkHashidsContext salt minLen alphabet
      encode ctx input `shouldBe` output

main :: IO ()
main = hspec $ do
  describe "hashids" $ do
      testSmallAlphabet
      testAlphabetWithSpaces
      testBadInput
      testAlphabet
      testSalt
      testMinLength
      testDefaultParams
      testDefaultParamsHex
      testAlphabet
      testJsHashidsCompatibleExamples
      testCustomParams
      testCustomParamsHex
      testBigNumberEncode
      testBigNumberDecode

