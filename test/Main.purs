module Test.Main where

import Prelude hiding (not)

import Data.Array (foldMap)
import Data.Array.NonEmpty (cons')
import Data.Foldable (fold)
import Data.Int (base36)
import Data.Maybe (Maybe(..), fromJust, fromMaybe, isJust, isNothing)
import Data.Monoid.Conj (Conj(..))
import Data.Newtype (un)
import Data.Ord (abs)
import Effect (Effect)
import Effect.Console (log)
import JS.BigInt (BigInt, and, asIntN, asUintN, binary, decimal, even, fromInt, fromNumber, fromString, fromStringAs, fromTLInt, hexadecimal, not, octal, odd, or, pow, shl, shr, toInt, toNumber, toString, toStringAs, xor)
import Partial.Unsafe (unsafePartial)
import Test.Assert (assert)
import Test.QuickCheck (quickCheck)
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Gen (Gen, arrayOf, chooseInt, elements, resize)
import Test.QuickCheck.Laws.Data as Data
import Type.Proxy (Proxy(..))

-- | Newtype with an Arbitrary instance that generates only small integers
newtype SmallInt = SmallInt Int

instance Arbitrary SmallInt where
  arbitrary = SmallInt <$> chooseInt (-5) 5

runSmallInt :: SmallInt -> Int
runSmallInt (SmallInt n) = n

-- | Arbitrary instance for BigInt
newtype TestBigInt = TestBigInt BigInt

derive newtype instance Eq TestBigInt
derive newtype instance Ord TestBigInt
derive newtype instance Semiring TestBigInt
derive newtype instance Ring TestBigInt
derive newtype instance CommutativeRing TestBigInt
derive newtype instance EuclideanRing TestBigInt

instance Arbitrary TestBigInt where
  arbitrary = testBigIntGen 100

-- | The generated digit strings are bounded by the requested size.
testBigIntGen :: Int -> Gen TestBigInt
testBigIntGen bound = do
  n <- (fromMaybe zero <<< fromString) <$> digitString
  op <- elements (cons' identity [ negate ])
  pure (TestBigInt (op n))
  where
  digits :: Gen Int
  digits = chooseInt 0 9

  digitString :: Gen String
  digitString = (fold <<< map show) <$> (resize bound $ arrayOf digits)

-- | Convert SmallInt to BigInt
fromSmallInt :: SmallInt -> BigInt
fromSmallInt = fromInt <<< runSmallInt

-- | Test if a binary relation holds before and after converting to BigInt.
testBinary
  :: (BigInt -> BigInt -> BigInt)
  -> (Int -> Int -> Int)
  -> Effect Unit
testBinary f g = quickCheck (\x y -> (fromInt x) `f` (fromInt y) == fromInt (x `g` y))

-- The 256-bit fixture must come from the parser, not from a silent fallback.
maxUint256 :: BigInt
maxUint256 = unsafePartial $ fromJust $ fromString "115792089237316195423570985008687907853269984665640564039457584007913129639935"

main :: Effect Unit
main = do
  log "Simple arithmetic operations and conversions from Int"
  let two = one + one
  let three = two + one
  let four = three + one
  assert $ fromInt 3 == three
  assert $ two * two == four
  assert $ two * three * (three + four) == fromInt 42
  assert $ two - three == fromInt (-1)

  log "Parsing strings"
  assert $ fromString "2" == Just two
  assert $ fromString "a" == Nothing
  assert $ fromString "2.1" == Nothing
  assert $ fromString "" == Just zero
  assert $ fromString "123456789" == Just (fromInt 123456789)
  assert $ fromString "10000000" == Just (fromInt 10000000)
  quickCheck $ \(TestBigInt a) -> (fromString <<< toString) a == Just a

  quickCheck $ \(TestBigInt a) ->
    let radixes = [binary, octal, decimal, hexadecimal, base36]
    in un Conj $ flip foldMap radixes $ \r ->
          Conj $ (fromStringAs r $ toStringAs r a) == Just a

  assert $ fromString "0b100" == Just four
  assert $ fromString "0o755" == Just (fromInt 493)
  assert $ fromString "0xff" == fromString "255"
  assert $ fromString "+12" == Just (fromInt 12)
  assert $ fromString " 12 " == Just (fromInt 12)
  assert $ fromString "0X1E" == Just (fromInt 30)
  -- JavaScript rejects signs on prefixed literals and exponent notation.
  assert $ fromString "-0x10" == Nothing
  assert $ fromString "-0b10" == Nothing
  assert $ fromString "1_000" == Nothing
  assert $ fromString "5e1" == Nothing
  assert $ fromString "1e3" == Nothing
  assert $ fromString "175e-2" == Nothing

  log "Parsing strings with a different base"
  assert $ fromStringAs binary "100" == Just four
  assert $ fromStringAs hexadecimal "ff" == Just (fromInt 255)
  assert $ fromStringAs hexadecimal "-ff" == Just (fromInt (-255))
  assert $ fromStringAs decimal "42" == Just (fromInt 42)
  assert $ fromStringAs base36 "z" == Just (fromInt 35)
  assert $ fromStringAs binary "2" == Nothing

  log "Round trips through every radix"
  let
    roundTrip :: BigInt -> Boolean
    roundTrip value =
      fromStringAs binary (toStringAs binary value) == Just value
        && fromStringAs octal (toStringAs octal value) == Just value
        && fromStringAs decimal (toStringAs decimal value) == Just value
        && fromStringAs hexadecimal (toStringAs hexadecimal value) == Just value
        && fromStringAs base36 (toStringAs base36 value) == Just value
  assert $ roundTrip (fromInt 0)
  assert $ roundTrip (fromInt 42)
  assert $ roundTrip (fromInt (-42))
  assert $ roundTrip maxUint256

  log "Conversions between String, Int and BigInt should not loose precision"
  quickCheck (\n -> fromString (show n) == Just (fromInt n))
  assert $ toStringAs binary (fromInt 9) == "1001"
  assert $ toStringAs octal (fromInt 10) == "12"

  log "Conversions between Number and BigInt"
  assert $ fromNumber 42.0 == Just (fromInt 42)
  assert $ fromNumber (-42.0) == Just (fromInt (-42))
  assert $ fromNumber 42.5 == Nothing
  assert $ isNothing (fromNumber (1.0 / 0.0))
  assert $ toNumber (fromInt 42) == 42.0
  assert $ toString (fromInt 9) == "9"

  log "Binary relations between integers should hold before and after converting to BigInt"
  testBinary (+) (+)
  testBinary (-) (-)
  testBinary mod mod
  testBinary (/) (/)

  log "Can parse 256 bit numbers"
  assert $ isJust $ fromString "115792089237316195423570985008687907853269984665640564039457584007913129639935"
  assert $ isJust $ fromStringAs hexadecimal "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0cbf"

  -- To test the multiplication, we need to make sure that Int does not overflow
  quickCheck (\x y -> fromSmallInt x * fromSmallInt y == fromInt (runSmallInt x * runSmallInt y))

  log "It should perform multiplications which would lead to imprecise results using Number"
  assert $ Just (fromInt 333190782 * fromInt 1103515245) == fromString "367681107430471590"

  log "compare, (==), even, odd should be the same before and after converting to BigInt"
  quickCheck (\x y -> compare x y == compare (fromInt x) (fromInt y))
  quickCheck (\x y -> (fromSmallInt x == fromSmallInt y) == (runSmallInt x == runSmallInt y))

  log "pow should perform integer exponentiation and yield 0 for negative exponents"
  assert $ three `pow` four == fromInt 81
  assert $ three `pow` -two == zero
  assert $ three `pow` zero == one
  assert $ zero `pow` zero == one

  log "Logic"
  assert $ (not <<< not) one == one
  assert $ not zero == fromInt (-1)
  assert $ or one three == three
  assert $ xor one three == two
  assert $ and one three == one

  log "Division and modulo follow JavaScript BigInt semantics"
  -- EuclideanRing division, like the JavaScript wrapper: div/mod satisfy
  -- `x = y * div x y + mod x y` with a non-negative remainder.
  assert $ three / two == one
  assert $ (fromInt (-3)) / two == fromInt (-2)
  assert $ three / (fromInt (-2)) == fromInt (-1)
  assert $ (fromInt (-3)) / (fromInt (-2)) == two
  assert $ three `mod` two == one
  assert $ (fromInt (-3)) `mod` two == one
  assert $ three `mod` fromInt (-2) == one
  assert $ (three / zero) == zero
  assert $ (three `mod` zero) == zero

  log "Shifting"
  assert $ shl two one == four
  assert $ shr two one == one
  assert $ shl two (fromInt (-1)) == one
  assert $ shr (fromInt (-2)) one == fromInt (-1)

  log "Wrapping with asIntN and asUintN"
  assert $ asUintN 8 (fromInt 255) == fromInt 255
  assert $ asUintN 8 (fromInt 256) == zero
  assert $ asUintN 8 (fromInt (-1)) == fromInt 255
  assert $ asIntN 8 (fromInt 127) == fromInt 127
  assert $ asIntN 8 (fromInt 128) == fromInt (-128)
  assert $ asIntN 8 (fromInt 255) == fromInt (-1)
  assert $ asIntN 8 (fromInt (-129)) == fromInt 127

  let prxBigInt = Proxy :: Proxy TestBigInt
  Data.checkEq prxBigInt
  Data.checkOrd prxBigInt
  Data.checkSemiring prxBigInt
  Data.checkRing prxBigInt
  Data.checkCommutativeRing prxBigInt
  -- The JavaScript wrapper's `degree` returned a BigInt for the declared
  -- `Int` result; the native port saturates at the 64-bit carrier. Run the
  -- Euclidean law where the degree comparisons are exact...
  Data.checkEuclideanRingGen (testBigIntGen 18)
  -- ... and keep the quotient/remainder identity over the full generated
  -- range, which only needs exact magnitudes.
  quickCheck
    \(TestBigInt a) (TestBigInt b) ->
      b == zero
        || ( let
               q = a / b
               r = a `mod` b
             in
               a == q * b + r && (r == zero || abs r < abs b)
           )

  log "Converting BigInt to Int"
  assert $ (fromString "0" >>= toInt) == Just 0
  assert $ (fromString "2137" >>= toInt) == Just 2137
  assert $ (fromString "-2137" >>= toInt) == Just (-2137)
  assert $ (fromString "921231231322337203685124775809" >>= toInt) == Nothing
  assert $ (fromString "-922337203612312312312854775809" >>= toInt) == Nothing

  log "Type Level Int creation"
  assert $ toString (fromTLInt (Proxy :: Proxy 921231231322337203685124775809)) == "921231231322337203685124775809"
  assert $ toString (fromTLInt (Proxy :: Proxy (-921231231322337203685124775809))) == "-921231231322337203685124775809"

  log "Parity"
  assert $ even (fromInt 42)
  assert $ odd (fromInt 42) == false
  assert $ odd (fromInt 31)
  assert $ even (fromInt 31) == false

  log "Tests passed"
