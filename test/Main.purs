module Test.Main where

import Prelude

import Data.Int (base36)
import Data.Maybe (Maybe(..), isJust, isNothing)
import Effect (Effect)
import Effect.Console (log)
import JS.BigInt (BigInt, and, asIntN, asUintN, binary, decimal, even, fromInt, fromNumber, fromString, fromStringAs, fromTLInt, hexadecimal, not, octal, odd, or, pow, shl, shr, toInt, toNumber, toString, toStringAs, xor)
import Test.Assert (assert)
import Type.Proxy (Proxy(..))

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
  assert $ roundTrip (fromString' "115792089237316195423570985008687907853269984665640564039457584007913129639935")

  log "Conversions between Number and BigInt"
  assert $ fromNumber 42.0 == Just (fromInt 42)
  assert $ fromNumber (-42.0) == Just (fromInt (-42))
  assert $ fromNumber 42.5 == Nothing
  assert $ isNothing (fromNumber (1.0 / 0.0))
  assert $ toNumber (fromInt 42) == 42.0
  assert $ toString (fromInt 9) == "9"

  log "It should perform multiplications which would lead to imprecise results using Number"
  assert $ Just (fromInt 333190782 * fromInt 1103515245) == fromString "367681107430471590"

  log "Can parse 256 bit numbers"
  assert $ isJust $ fromString "115792089237316195423570985008687907853269984665640564039457584007913129639935"
  assert $ isJust $ fromStringAs hexadecimal "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0cbf"

  log "pow should perform integer exponentiation and yield 0 for negative exponents"
  assert $ three `pow` four == fromInt 81
  assert $ three `pow` (-two) == zero
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

  log "compare, (==), even, odd should be the same before and after converting to BigInt"
  assert $ compare (fromInt 2) (fromInt 3) == compare 2 3
  assert $ fromInt 4 == fromInt 4
  assert $ fromInt 4 /= fromInt 5
  assert $ even (fromInt 42)
  assert $ odd (fromInt 42) == false
  assert $ odd (fromInt 31)
  assert $ even (fromInt 31) == false

  log "Converting BigInt to Int"
  assert $ (fromString "0" >>= toInt) == Just 0
  assert $ (fromString "2137" >>= toInt) == Just 2137
  assert $ (fromString "-2137" >>= toInt) == Just (-2137)
  assert $ (fromString "921231231322337203685124775809" >>= toInt) == Nothing
  assert $ (fromString "-922337203612312312312854775809" >>= toInt) == Nothing

  log "Type Level Int creation"
  assert $ toString (fromTLInt (Proxy :: Proxy 921231231322337203685124775809)) == "921231231322337203685124775809"
  assert $ toString (fromTLInt (Proxy :: Proxy (-921231231322337203685124775809))) == "-921231231322337203685124775809"

  log "Tests passed"
  where
  fromString' text = case fromString text of
    Just value -> value
    Nothing -> zero
