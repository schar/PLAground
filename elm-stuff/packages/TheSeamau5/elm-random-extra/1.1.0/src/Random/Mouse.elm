module Random.Mouse where
{-| List of Mouse Generators

# Generators
@docs mousePosition, mouseX, mouseY, leftMouseDown

-}

import Random (..)
import Random.Extra (bool, zip)

{-| Generate a random mouse position given a screen width and a screen height
-}
mousePosition : Int -> Int -> Generator (Int, Int)
mousePosition screenWidth screenHeight =
  zip (int 0 screenWidth) (int 0 screenHeight)

{-| Generate a random mouseX value given a screen width
-}
mouseX : Int -> Generator Int
mouseX screenWidth =
  int 0 screenWidth

{-| Generate a random mouseY value given a screen height
-}
mouseY : Int -> Generator Int
mouseY screenHeight =
  int 0 screenHeight

{-| Generate a random instance of left mouse down (alias for `bool`)
-}
leftMouseDown : Generator Bool
leftMouseDown = bool
