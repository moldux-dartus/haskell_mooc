module Set16b where

import Mooc.Todo
import Examples.Phantom

import Data.Char (toUpper)

------------------------------------------------------------------------------
-- Ex 1: Define a constant pounds with type Money GBP and a value of
-- 3. The type Money is imported from Example.Phantom but you'll need
-- to introduce GBP yourself.

data GBP

pounds :: Money GBP
pounds = Money 3

------------------------------------------------------------------------------
-- Ex 2: Implement composition for Rates. Give composeRates a
-- restricted type so that the currencies are tracked correctly.
--
-- Examples:
--   composeRates (Rate 1.5) (Rate 1.25) ==> Rate 1.875
--   composeRates eurToUsd usdToChf :: Rate EUR CHF
--   composeRates eurToUsd (invert eurToUsd) :: Rate EUR EUR
--   composeRates eurToUsd eurToUsd :: type error!
--   composeRates eurToUsd :: Rate USD to -> Rate EUR to

-- For testing
usdToChf :: Rate USD CHF
usdToChf = Rate 1.11

composeRates :: Rate c1 c2 -> Rate c2 c3 -> Rate c1 c3
composeRates (Rate r1) (Rate r2) = Rate (r1 * r2)

------------------------------------------------------------------------------
-- Ex 3: Tracking first, last and full names with phantom types. The
-- goal is to have the types:
--  * Name First - for first names
--  * Name Last - for last names
--  * Name Full - for full names
--
-- In this exercise, you should define the types First, Last, Full and
-- Name. Then implement the functions fromName, toFirst and toLast. Give
-- the functions the commented-out types
--
-- Examples:
--  fromName (toFirst "bob") ==> "bob"
--  fromName (toLast "smith") ==> "smith"
--  toFirst "bob" :: Name First
--  toLast "smith" :: Name Last

data Name name = Name String
data First
data Last
data Full

-- Get the String contained in a name
fromName :: Name a -> String
fromName (Name n) = n

-- Build a Name First
toFirst :: String -> Name First
toFirst = Name

-- Build a Name Last
toLast :: String -> Name Last
toLast = Name

------------------------------------------------------------------------------
-- Ex 4: Implement the functions capitalize and toFull.
--
-- toFull should combine a first and a last name into a full name. Give
-- toFull the correct type (see examples below).
--
-- capitalize shouldCapitalize the first letter of a name. Give
-- capitalize the correct type (see examples below).
--
-- Examples:
--  toFull (toFirst "bob") (toLast "smith") :: Name Full
--  fromName (toFull (toFirst "bob") (toLast "smith"))
--    ==> "bob smith"
--  capitalize (toFirst "bob") :: Name First
--  fromName (capitalize (toFirst "bob")) ==> "Bob"
--  capitalize (toLast "smith") :: Name Last
--  fromName (capitalize (toLast "smith")) ==> "Smith"

capitalize :: Name name -> Name name
capitalize (Name (x:xs))= Name (toUpper x:xs)

toFull :: Name First -> Name Last -> Name Full
toFull (Name f) (Name l) = Name (f ++ " " ++ l)

------------------------------------------------------------------------------
-- Ex 5: Type classes can let you write code that handles different
-- phantom types differently. Define instances for the Render type
-- class such that:
--
--  render (Money 1.0 :: Money EUR) ==> "1.0e"
--  render (Money 1.0 :: Money USD) ==> "$1.0"
--  render (Money 1.0 :: Money CHF) ==> "1.0chf"

class Render currency where
  render :: Money currency -> String

instance Render EUR where
  render (Money eur) = show eur ++ "e"

instance Render CHF where
  render (Money chf) = show chf ++ "chf"

instance Render USD where
  render (Money usd) = "$" ++ show usd
