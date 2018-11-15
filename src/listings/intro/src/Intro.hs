{-# LANGUAGE OverloadedStrings #-}
module Intro where

import           Data.Text
import           Data.Text.IO as Text


-- start snippet product-type-1
data Customer =
  Customer
  { firstName :: Text
  , lastName  :: Text
  }
-- end snippet product-type-1

-- start snippet sum-type-1
data MealPreference
  = Omnivore
  | OvoLacto
  | Vegetarian
-- end snippet sum-type-1

-- start snippet function-1
fullName :: Customer -> Text
fullName customer =
  firstName customer <> " " <> lastName customer
-- end snippet function-1

-- start snippet pattern-matching
data Meal
  = BurgerAndFries
  | QuattroFormaggio
  | ChickpeaCurry

formatMeal :: Meal -> Text
formatMeal meal = case meal of
  BurgerAndFries   -> "Burger and Fries"
  QuattroFormaggio -> "Quattro Formaggio"
  ChickpeaCurry    -> "Chickpea Curry"
-- end snippet pattern-matching

-- start snippet nested-data
data Order = Order { orderCustomer :: Customer
                   , orderMeal     :: Meal
                   }

airlineStyleOrder :: Customer -> MealPreference -> Order
airlineStyleOrder customer pref =
  case pref of
    Omnivore   -> Order customer BurgerAndFries
    OvoLacto   -> Order customer QuattroFormaggio
    Vegetarian -> Order customer ChickpeaCurry
-- end snippet nested-data

-- start snippet effects
printOrder :: Order -> IO ()
printOrder order =
  let msg =
        fullName (orderCustomer order)
          <> " ordering "
          <> formatMeal (orderMeal order)
          <> "."
  in Text.putStrLn msg
-- end snippet effects
