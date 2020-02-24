{-# LANGUAGE BangPatterns #-}

module Hashids 
  ( HashidsError(..)
  , HashidsContext
  , mkHashidsContext
  , defaultAlphabet
  , encode
  , encodeHex
  , decode 
  , decodeHex
  ) where
  
import Control.Monad (join, when, forM_)
import qualified Data.List as List
import Data.Array.ST.Safe
import qualified Data.Array.MArray as MArray
import qualified Data.Array as Array
import Control.Monad.ST
import Data.STRef 
import Data.Array ((!))
import qualified Data.Char as Char
import qualified Data.Maybe as Maybe
import Data.Ratio ((%))
import Data.String (String)
import Numeric
import qualified Data.Maybe as Maybe

data HashidsError 
  = AlphabetToSmall
  | AlphabetContainsSpaces
  deriving (Eq, Show)

data HashidsContext = HashidsContext
  { _hSalt :: !String
  , _hMinHashLength :: !Integer
  , _hAlphabet :: !String
  , _hSeps :: !String
  , _hGuards :: !String
  } deriving (Eq, Show)

-- Minimum alphabet length for use in hashids
minAlphabetLength :: Integer
minAlphabetLength = 16

-- Seperators specified by the hashids algorithm
seperatorsList :: String 
seperatorsList = "cfhistuCFHISTU";

seperatorDivisor :: Rational
seperatorDivisor = 7 % 2 

guardDivisor :: Rational
guardDivisor = 12

defaultAlphabet :: String
defaultAlphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"

-- Make the context and do verification to ensure that the alphabet is valid
-- this will shuffle the alphabet and seperators using the salt provided
-- it will ensure a certain number of seperator characters and "guard" characters
-- are reserved for preventing curse words and ?storing arrays?
mkHashidsContext :: String -> Integer -> String -> Either HashidsError HashidsContext
mkHashidsContext salt minLength alphabet = do 
  let uniqueAlphabet = List.nub alphabet

  when (toInteger (length uniqueAlphabet) < minAlphabetLength) $
    Left AlphabetToSmall

  when (length (words uniqueAlphabet) > 1) $
    Left AlphabetContainsSpaces 

  let sepsToUse       = List.intersect seperatorsList uniqueAlphabet
      cleanedAlphabet = uniqueAlphabet `arrayDiff` sepsToUse
      shuffledSeps    = shuffle sepsToUse salt
      (balancedSeps, balancedAlphabet) = balanceSepsAndAlphabet shuffledSeps cleanedAlphabet
      shuffledAlphabet = shuffle balancedAlphabet salt
      guardCount      = ceiling $ ((toRational $ length shuffledAlphabet) / guardDivisor)
  
  if length shuffledAlphabet < 3 then do
    let guards = take guardCount balancedSeps
        finalSeps = drop guardCount balancedSeps
    pure $ HashidsContext salt minLength shuffledAlphabet finalSeps guards
  else do
    let guards = take guardCount shuffledAlphabet 
        finalAlphabet = drop guardCount shuffledAlphabet
    pure $ HashidsContext salt minLength finalAlphabet balancedSeps guards

 where
  arrayDiff l1 l2 =
    List.filter (`notElem` l2) l1

  balanceSepsAndAlphabet shuffledSeps cleanedAlphabet =
    let sepsLen         = length shuffledSeps
        alphabetLen     = length cleanedAlphabet
    in
    if (null shuffledSeps || ((toRational alphabetLen) / (toRational sepsLen)) > seperatorDivisor) then
      let calculatedSepsLength = toInteger $ ceiling (toRational (length cleanedAlphabet) / seperatorDivisor)
      in
      if calculatedSepsLength > toInteger sepsLen then
        let diff = calculatedSepsLength - (toInteger sepsLen)
            balancedSeps = shuffledSeps <> take (fromIntegral diff) cleanedAlphabet 
            balancedAlphabet = drop (fromIntegral diff) cleanedAlphabet
        in (balancedSeps, balancedAlphabet)
      else 
        (shuffledSeps, cleanedAlphabet)
    else 
      (shuffledSeps, cleanedAlphabet)


  
-- Implementing this using ST so that the translation is easier. 
-- A more idiomatic implementation probably exists but vOv
encode :: HashidsContext -> [Integer] -> String 
encode context unfilteredNumbers = 
  if null numbers then
    ""
  else runST $ do
  ret <- newSTRef ""
  alphabetRef <- newSTRef $ _hAlphabet context
  let numbersSize = toInteger $ length numbers

  -- Generate the numbersHashInteger, this can probably be rewritten as a fold
  numbersHashIntegerRef <- newSTRef (0 :: Integer)
  forM_ indexedNumbers $ \(number, i) -> do
    modifySTRef' numbersHashIntegerRef $ \numbersHashInteger -> 
      numbersHashInteger + ((number `mod` (toInteger i + 100)))
  numbersHashInteger <- readSTRef numbersHashIntegerRef 
  alphabet' <- readSTRef alphabetRef
  let lottery = take 1 $ drop (fromIntegral $ numbersHashInteger `mod` toInteger (length alphabet')) alphabet'
  writeSTRef ret lottery

  forM_ indexedNumbers $ \(number, i) -> do
    modifySTRef' alphabetRef $ \alphabet ->
      shuffle alphabet (take (length alphabet) (lottery <> (_hSalt context) <> alphabet))
    currentAlphabet <- readSTRef alphabetRef 
    let last = hash number currentAlphabet
    modifySTRef' ret (\r -> r <> last)

    when ((toInteger i + 1) < numbersSize) $ do
      let moddedNumber = number `mod` (toInteger (Char.ord (head last)) + i)
          sepsIndex = moddedNumber `mod` toInteger (length (_hSeps context))
      modifySTRef' ret (\r -> r <> (take 1 $ drop (fromIntegral sepsIndex) $ _hSeps context))
    
  ret' <- readSTRef ret
  when (toInteger (length ret') < _hMinHashLength context) $ do
    let guardIndex = (numbersHashInteger + toInteger (Char.ord $ head ret')) `mod` toInteger (length (_hGuards context))
        guard = take 1 $ drop (fromIntegral guardIndex) $ _hGuards context
    modifySTRef' ret (\r -> guard <> r)
    ret'' <- readSTRef ret
    when (toInteger (length ret'') < _hMinHashLength context) $ do
      let guardIndex' = (numbersHashInteger + toInteger (Char.ord $ head $ drop 2 ret'')) `mod` toInteger (length (_hGuards context))
          guard' = take 1 $ drop (fromIntegral guardIndex') $ _hGuards context
      modifySTRef' ret (\r -> r <> guard')

  shuffledAlphabet <- readSTRef alphabetRef 
  let halfLength = toInteger (length shuffledAlphabet) `div` 2 

  let checkRetLength r = toInteger (length r) < (_hMinHashLength context)

  whileRef_ ret checkRetLength $ do
    modifySTRef' alphabetRef $ \alphabet -> shuffle alphabet alphabet
    alphabet <- readSTRef alphabetRef
    modifySTRef' ret $ \ret' -> 
      drop (fromIntegral halfLength) alphabet <> ret' <> take (fromIntegral halfLength) alphabet
    ret' <- readSTRef ret
    let excess = toInteger (length ret') - (_hMinHashLength context)
    when (excess > 0) $ do
      modifySTRef' ret $ \r ->
        take (fromIntegral $ _hMinHashLength context) $ drop (fromIntegral (excess `div` 2)) r

  readSTRef ret

  where
    numbers = List.filter (>= 0) unfilteredNumbers
    indexedNumbers = (zip numbers [0..])

    whileRef_ ref cond action = do
      v <- readSTRef ref
      when (cond v) $ do
        action
        whileRef_ ref cond action


encodeHex :: HashidsContext -> String -> String 
encodeHex context numbers = 
  if all Char.isHexDigit numbers then do
    let hexChunks = chunksOf 12 numbers
        hexNumbers = fmap fst $ join $ fmap (readHex . ("1" <>)) hexChunks
    encode context hexNumbers
  else ""
    where
      chunksOf chunkLength body =
        if null body then
          []
        else 
        let front = take chunkLength body
            back  = drop chunkLength body
        in front : chunksOf chunkLength back

decode :: HashidsContext -> String -> [Integer]
decode context hash_ = runST $ 
  if length hash == 0 then
    pure []
  else do
    ret <- newSTRef []
    alphabetRef <- newSTRef $ _hAlphabet context
    let hashList      = splitOneOf (_hGuards context) hash
        i             = if (length hashList == 3 || length hashList == 2) then 1 else 0
        hashBreakdown = hashList !! i
    when (not $ null hashBreakdown) $ do
      let lottery = take 1 hashBreakdown
          hashBreakdown' = drop 1 hashBreakdown
          hashList' = splitOneOf (_hSeps context) hashBreakdown'
      forM_ hashList' $ \subHash -> do    
        modifySTRef' alphabetRef $ \alphabet ->
          shuffle alphabet (take (length alphabet) (lottery <> (_hSalt context) <> alphabet))
        alphabet' <- readSTRef alphabetRef
        let result = unhash subHash alphabet'
        modifySTRef ret (++ [result])
    res <- Maybe.catMaybes <$> readSTRef ret 
    if encode context res /= hash then
      pure []
    else
      pure res
    where
      hash = trim hash_

      -- Probably could be more efficient
      trim = List.dropWhile Char.isSpace . List.dropWhileEnd Char.isSpace
      splitOneOf delimeters str =
        if length str > 0 then
          let front = List.takeWhile (\c -> notElem c delimeters) str
              back = List.drop 1 $ List.dropWhile (\c -> notElem c delimeters) str
              result = front : splitOneOf delimeters back
          in result
        else
          [] 

unhash :: String -> String -> Maybe Integer
unhash input alphabet = 
  if not (null input || null alphabet) then 
    go 0 input
  else 
    Nothing
  where
  go !number chars =
    case chars of
      [] -> Just number
      c:cs -> 
        (List.elemIndex c alphabet) >>= \position ->
        let nextNumber = (number * (toInteger $ length alphabet)) + toInteger position
        in go nextNumber cs


decodeHex :: HashidsContext -> String -> String
decodeHex context hash = 
  let result = decode context hash 
   in concat $ fmap (\i -> drop 1 $ showHex i "") result

hash :: Integer -> String -> String
hash number alphabet = go number "" 
  where
    alphabetLength = toInteger $ length alphabet
    go !input !hash =
      let nextHash = take 1 $ drop (fromIntegral (input `mod` alphabetLength)) alphabet 
          nextInput = input `div` alphabetLength
      in 
      if nextInput > 0 then
        go nextInput (nextHash <> hash)
      else 
        nextHash <> hash

-- Uses the hashids consistent shuffle algorithm to shuffle the given alphabet
-- This algorithm does a lot of swapping and random access so the code is implemented
-- in the ST monad
shuffle :: String -> String -> String
shuffle alphabet salt =
  if saltLen == 0 then
    alphabet
  else
    Array.elems $ runSTArray $ do
      -- We need a mutable array to support swap
      stAlphabet <- MArray.newListArray (0, alphabetLen) alphabet
      go stAlphabet alphabetLen 0 0

  where
    saltLen = toInteger $ length salt
    saltArr = Array.listArray (0, saltLen - 1) salt
    alphabetLen = toInteger (length alphabet) - 1

    go alphabetArray !currentPosition !saltPosition !p =
      if currentPosition == 0 then
        pure alphabetArray
      else do
            -- In case we have reached the end of the salt we need to loop back around
        let nextSaltPosition = saltPosition `mod` saltLen
            -- the algorithm uses the codepoint value of the characters in the salt
            saltCharacterValue = toInteger $ Char.ord (saltArr ! fromIntegral nextSaltPosition)
            -- We increment p by the character value to add more entropy(maybe?)
            nextP = p + saltCharacterValue 
            -- we dont want to ruin the work we already did on the array so modulo the current position
            swapPosition = (saltCharacterValue + nextSaltPosition + nextP) `mod` currentPosition
        -- Swap the characters in the alphabet and then examine then iterate
        swap alphabetArray currentPosition swapPosition
        go alphabetArray (currentPosition - 1) (nextSaltPosition + 1) nextP

    swap arr currentPosition swapPosition = do
        tmp <- MArray.readArray arr swapPosition
        currentChar <- MArray.readArray arr currentPosition
        MArray.writeArray arr swapPosition currentChar
        MArray.writeArray arr currentPosition tmp
