module Random.Extra
  ( constant
  , emptyList
  , rangeLengthList
  , bool
  , anyInt
  , positiveInt
  , negativeInt
  , intGreaterThan
  , intLessThan
  , anyFloat
  , positiveFloat
  , negativeFloat
  , floatGreaterThan
  , floatLessThan
  , probability
  , negativeProbability
  , absoluteProbability
  , func
  , func2
  , func3
  , func4
  , func5
  , func6
  , apply
  , (<<<)
  , (>>>)
  , select
  , selectWithDefault
  , map
  , map2
  , map3
  , map4
  , map5
  , map6
  , flatMap
  , flatMap2
  , flatMap3
  , flatMap4
  , flatMap5
  , flatMap6
  , zip
  , zip3
  , zip4
  , zip5
  , zip6
  , andThen
  , merge
  , quickGenerate
  , cappedGenerateUntil
  , generateIterativelyUntil
  , generateIterativelySuchThat
  , generateUntil
  , generateSuchThat
  , maybeGenerateSuchThat
  )where
{-| Module providing extra functionality to the core Random module.

# Constant Generators
@docs constant, emptyList

# Boolean Generator
@docs bool

# Integer Generators
@docs anyInt, positiveInt, negativeInt, intGreaterThan, intLessThan

# Float Generators
@docs anyFloat, positiveFloat, negativeFloat, floatGreaterThan, floatLessThan, probability, negativeProbability, absoluteProbability

# List Generators
@docs rangeLengthList

# Function Generators
@docs func, func2, func3, func4, func5, func6

# Operations on Function Generators
@docs apply, (<<<), (>>>)

# Select
@docs select, selectWithDefault

# Maps
@docs map, map2, map3, map4, map5, map6, mapConstraint

# Flat Maps
@docs flatMap, flatMap2, flatMap3, flatMap4, flatMap5, flatMap6

# Zips
@docs zip, zip3, zip4, zip5, zip6

# Chaining Generators
@docs andThen

# Merging Generators
@docs merge

# Generate Functions
@docs quickGenerate, cappedGenerateUntil, generateIterativelyUntil, generateIterativelySuchThat, generateUntil, maybeGenerateSuchThat, generateSuchThat

-}

import Random (..)
import List


get : Int -> List a -> Maybe a
get index list =
  if index < 0
  then Nothing
  else
    case List.drop index list of
      [] -> Nothing
      x :: xs -> Just x


{-| Generator that always returns the empty list.
-}
emptyList : Generator (List a)
emptyList =
  constant []

{-| Generate a random list of random length given a minimum length and
a maximum length.
-}
rangeLengthList : Int -> Int -> Generator a -> Generator (List a)
rangeLengthList minLength maxLength generator =
  flatMap (\len -> list len generator) (int minLength maxLength)

{-| Generator that randomly selects an element from a list.
-}
select : List a -> Generator (Maybe a)
select list =
  customGenerator <|
    (\seed ->
        let (index, nextSeed) = generate (int 0 (List.length list - 1)) seed
        in
          (get index list, nextSeed))


{-| Generator that randomly selects an element from a list with a default value
(in case you pass in an empty list).
-}
selectWithDefault : a -> List a -> Generator a
selectWithDefault defaultValue list =
  customGenerator <|
    (\seed ->
        let (index, nextSeed) = generate (int 0 (List.length list - 1)) seed
        in
          case get index list of
            Nothing     -> (defaultValue, nextSeed)
            Just value  -> (value, nextSeed))




{-| Create a generator that always returns the same value.
-}
constant : a -> Generator a
constant value =
  customGenerator
    (\seed ->
        let (_, seed1) = generate (int 0 1) seed
        in
          (value, seed1))



{-| Random Bool generator
-}
bool : Generator Bool
bool =
  customGenerator
    (\seed ->
        let (value1, seed1) = generate (int 0 1) seed
            (value2, seed2) = generate (int 0 1) seed1
        in
          if (value1 + value2) == 1
          then
            (False, seed2)
          else
            (True, seed2))



apply : Generator (a -> b) -> Generator a -> Generator b
apply funcGenerator generator =
  customGenerator <|
    (\seed ->
        let (f, seed1) = generate funcGenerator seed
            (a, seed2) = generate generator seed1
        in
          ((f a), seed2))


{-| Generates a random function of one argument given a generator for the output.
-}
func : Generator b -> Generator (a -> b)
func generatorB =
  customGenerator <|
    (\seed ->
        let (valueB, seed1) = generate generatorB seed
        in
          ((\a -> valueB), seed1))


{-| Generates a random function of two arguments given a generator for the output.
-}
func2 : Generator c -> Generator (a -> b -> c)
func2 generatorC =
  func (func generatorC)


{-| Generates a random function of three arguments given a generator for the output.
-}
func3 : Generator d -> Generator (a -> b -> c -> d)
func3 generatorD =
  func (func2 generatorD)


{-| Generates a random function of four arguments given a generator for the output.
-}
func4 : Generator e -> Generator (a -> b -> c -> d -> e)
func4 generatorE =
  func (func3 generatorE)


{-| Generates a random function of five arguments given a generator for the output.
-}
func5 : Generator f -> Generator (a -> b -> c -> d -> e -> f)
func5 generatorF =
  func (func4 generatorF)


{-| Generates a random function of six arguments given a generator for the output.
-}
func6 : Generator g -> Generator (a -> b -> c -> d -> e -> f -> g)
func6 generatorG =
  func (func5 generatorG)


infixl 9 >>>
{-| Compose two function generators. Analogous to `>>`
-}
(>>>) : Generator (a -> b) -> Generator (b -> c) -> Generator (a -> c)
(>>>) generatorAB generatorBC =
  customGenerator <|
    (\seed ->
        let (f, seed1) = generate generatorAB seed
            (g, seed2) = generate generatorBC seed1
        in
          (f >> g, seed2))


infixr 9 <<<
{-| Compose two function generators. Analogous to `<<`
-}
(<<<) : Generator (b -> c) -> Generator (a -> b) -> Generator (a -> c)
(<<<) generatorBC generatorAB =
  customGenerator <|
    (\seed ->
        let (f, seed1) = generate generatorAB seed
            (g, seed2) = generate generatorBC seed1
        in
          (f >> g, seed2))

{-| Generator that generates any int that can be generate by the random generator algorithm.
-}
anyInt : Generator Int
anyInt = int minInt maxInt

{-| Generator that generates a positive int
-}
positiveInt : Generator Int
positiveInt = int 1 maxInt

{-| Generator that generates a negative int
-}
negativeInt : Generator Int
negativeInt = int minInt -1

{-| Generator that generates an int greater than a given int
-}
intGreaterThan : Int -> Generator Int
intGreaterThan value = int (value + 1) maxInt

{-| Generator that generates an int less than a given int
-}
intLessThan : Int -> Generator Int
intLessThan value = int minInt (value - 1)

{-| Generator that generates any float
-}
anyFloat : Generator Float
anyFloat = float (toFloat minInt) (toFloat maxInt)

{-| Generator that generates any positive float
-}
positiveFloat : Generator Float
positiveFloat = float 0 (toFloat maxInt)

{-| Generator that generates any negative float
-}
negativeFloat : Generator Float
negativeFloat = float (toFloat minInt) 0

{-| Generator that generates a float greater than a given float
-}
floatGreaterThan : Float -> Generator Float
floatGreaterThan value = float value (toFloat maxInt)

{-| Generator that generates a float less than a given float
-}
floatLessThan : Float -> Generator Float
floatLessThan value = float (toFloat minInt) value

{-| Generator that generates a float between 0 and 1
-}
probability : Generator Float
probability = float 0 1

{-| Generator that generates a float between -1 and 0
-}
negativeProbability : Generator Float
negativeProbability = float -1 0

{-| Generator that generates a float between - 1 and 1
-}
absoluteProbability : Generator Float
absoluteProbability = float -1 1

{-}
normal : Float -> Float -> Float -> Generator Float
normal start end standardDeviation =
  let normalDistribution mean stdDev x =
        if stdDev == 0 then x
        else
          let scale = 1 / (stdDev * sqrt (2 * pi))
              exponent = ((x - mean) * (x - mean)) / (2 * stdDev * stdDev)
          in
            scale * (e ^ -exponent)

  in
    map (normalDistribution ((end - start) / 2) standardDeviation) (float start end)

standardNormal : Generator Float
standardNormal = normal (toFloat minInt + 1) (toFloat maxInt) 1

gaussian : Float -> Float -> Float -> Generator Float
gaussian = normal

-}




zip : Generator a -> Generator b -> Generator (a, b)
zip = map2 (,)

zip3 : Generator a -> Generator b -> Generator c -> Generator (a, b, c)
zip3 = map3 (,,)

zip4 : Generator a -> Generator b -> Generator c -> Generator d -> Generator (a, b, c, d)
zip4 = map4 (,,,)

zip5 : Generator a -> Generator b -> Generator c -> Generator d -> Generator e -> Generator (a, b, c, d, e)
zip5 = map5 (,,,,)

zip6 : Generator a -> Generator b -> Generator c -> Generator d -> Generator e -> Generator f -> Generator (a, b, c, d, e, f)
zip6 = map6 (,,,,,)


andThen : Generator a -> (a -> Generator b) -> Generator b
andThen generator constructor =
  flatMap constructor generator

flatMap : (a -> Generator b) -> Generator a -> Generator b
flatMap constructor generator =
  customGenerator
    (\seed ->
        let (valueA, seed1) = generate generator seed
            generatorB = constructor valueA
        in
          generate generatorB seed1)

flatMap2 : (a -> b -> Generator c) -> Generator a -> Generator b -> Generator c
flatMap2 constructor generatorA generatorB =
  customGenerator
    (\seed ->
        let (valueA, seed1) = generate generatorA seed
            (valueB, seed2) = generate generatorB seed1
            generatorC = constructor valueA valueB
        in
          generate generatorC seed2)

flatMap3 : (a -> b -> c -> Generator d) -> Generator a -> Generator b -> Generator c -> Generator d
flatMap3 constructor generatorA generatorB generatorC =
  customGenerator
    (\seed ->
        let (valueA, seed1) = generate generatorA seed
            (valueB, seed2) = generate generatorB seed1
            (valueC, seed3) = generate generatorC seed2
            generatorD = constructor valueA valueB valueC
        in
          generate generatorD seed3)

flatMap4 : (a -> b -> c -> d -> Generator e) -> Generator a -> Generator b -> Generator c -> Generator d -> Generator e
flatMap4 constructor generatorA generatorB generatorC generatorD =
  customGenerator
    (\seed ->
        let (valueA, seed1) = generate generatorA seed
            (valueB, seed2) = generate generatorB seed1
            (valueC, seed3) = generate generatorC seed2
            (valueD, seed4) = generate generatorD seed3
            generatorE = constructor valueA valueB valueC valueD
        in
          generate generatorE seed4)


flatMap5 : (a -> b -> c -> d -> e -> Generator f) -> Generator a -> Generator b -> Generator c -> Generator d -> Generator e -> Generator f
flatMap5 constructor generatorA generatorB generatorC generatorD generatorE =
  customGenerator
    (\seed ->
        let (valueA, seed1) = generate generatorA seed
            (valueB, seed2) = generate generatorB seed1
            (valueC, seed3) = generate generatorC seed2
            (valueD, seed4) = generate generatorD seed3
            (valueE, seed5) = generate generatorE seed4
            generatorF = constructor valueA valueB valueC valueD valueE
        in
          generate generatorF seed5)


flatMap6 : (a -> b -> c -> d -> e -> f -> Generator g) -> Generator a -> Generator b -> Generator c -> Generator d -> Generator e -> Generator f -> Generator g
flatMap6 constructor generatorA generatorB generatorC generatorD generatorE generatorF =
  customGenerator
    (\seed ->
        let (valueA, seed1) = generate generatorA seed
            (valueB, seed2) = generate generatorB seed1
            (valueC, seed3) = generate generatorC seed2
            (valueD, seed4) = generate generatorD seed3
            (valueE, seed5) = generate generatorE seed4
            (valueF, seed6) = generate generatorF seed5
            generatorG = constructor valueA valueB valueC valueD valueE valueF
        in
          generate generatorG seed6)



map : (a -> b) -> Generator a -> Generator b
map f generator =
  customGenerator
    (\seed ->
        let (value, nextSeed) = generate generator seed
        in
          (f value, nextSeed))

map2 : (a -> b -> c) -> Generator a -> Generator b -> Generator c
map2 f generatorA generatorB =
  customGenerator
    (\seed ->
        let (valueA, seed1) = generate generatorA seed
            (valueB, seed2) = generate generatorB seed1
        in
          (f valueA valueB, seed2))

map3 : (a -> b -> c -> d) -> Generator a -> Generator b -> Generator c -> Generator d
map3 f generatorA generatorB generatorC =
  customGenerator
    (\seed ->
        let (valueA, seed1) = generate generatorA seed
            (valueB, seed2) = generate generatorB seed1
            (valueC, seed3) = generate generatorC seed2
        in
          (f valueA valueB valueC, seed3))

map4 : (a -> b -> c -> d -> e) -> Generator a -> Generator b -> Generator c -> Generator d -> Generator e
map4 f generatorA generatorB generatorC generatorD =
  customGenerator
    (\seed ->
        let (valueA, seed1) = generate generatorA seed
            (valueB, seed2) = generate generatorB seed1
            (valueC, seed3) = generate generatorC seed2
            (valueD, seed4) = generate generatorD seed3
        in
          (f valueA valueB valueC valueD, seed4))

map5 : (a -> b -> c -> d -> e -> f) -> Generator a -> Generator b -> Generator c -> Generator d -> Generator e -> Generator f
map5 f generatorA generatorB generatorC generatorD generatorE =
  customGenerator
    (\seed ->
        let (valueA, seed1) = generate generatorA seed
            (valueB, seed2) = generate generatorB seed1
            (valueC, seed3) = generate generatorC seed2
            (valueD, seed4) = generate generatorD seed3
            (valueE, seed5) = generate generatorE seed4
        in
          (f valueA valueB valueC valueD valueE, seed5))

map6 : (a -> b -> c -> d -> e -> f -> g) -> Generator a -> Generator b -> Generator c -> Generator d -> Generator e -> Generator f -> Generator g
map6 f generatorA generatorB generatorC generatorD generatorE generatorF =
  customGenerator
    (\seed ->
        let (valueA, seed1) = generate generatorA seed
            (valueB, seed2) = generate generatorB seed1
            (valueC, seed3) = generate generatorC seed2
            (valueD, seed4) = generate generatorD seed3
            (valueE, seed5) = generate generatorE seed4
            (valueF, seed6) = generate generatorF seed5
        in
          (f valueA valueB valueC valueD valueE valueF, seed6))

merge : Generator a -> Generator a -> Generator a
merge generator1 generator2 =
  customGenerator
    (\seed ->
        let value = quickGenerate bool seed
        in
          if value == True
          then
            generate generator1 seed
          else
            generate generator2 seed)


generateSuchThat : (a -> Bool) -> Generator a -> Seed -> (a, Seed)
generateSuchThat predicate generator seed =
  let (value, nextSeed) = generate generator seed
  in
    if predicate value
    then
      (value, nextSeed)
    else
      generateSuchThat predicate generator nextSeed

maybeGenerateSuchThat : Int -> (a -> Bool) -> Generator a -> Seed -> Maybe (a, Seed)
maybeGenerateSuchThat numberOfTries predicate generator seed =
  if numberOfTries <= 0
  then Nothing
  else
    let (value, nextSeed) = generate generator seed
    in
      if predicate value
      then
        Just (value, nextSeed)
      else
        maybeGenerateSuchThat (numberOfTries - 1) predicate generator nextSeed


generateUntil : (a -> Bool) -> Generator a -> Seed -> List a
generateUntil predicate generator seed =
  let (value, nextSeed) = generate generator seed
  in
    if predicate value
    then
      value :: generateUntil predicate generator nextSeed
    else
      []


generateIterativelySuchThat : Int -> (a -> Bool) -> (Int -> Generator a) -> Seed -> List a
generateIterativelySuchThat maxLength predicate constructor seed =
  let notPredicate = (\value -> not (predicate value))

      iterate index =
        if index >= maxLength
        then
          []
        else
          (generateUntil notPredicate (constructor index) seed) `List.append`
          (iterate (index + 1))

  in
    iterate 0


generateIterativelyUntil : Int -> (a -> Bool) -> (Int -> Generator a) -> Seed -> List a
generateIterativelyUntil maxLength predicate constructor seed =
  let iterate index =
        if index >= maxLength
        then
          []
        else
          (generateUntil predicate (constructor index) seed) `List.append`
          (iterate (index + 1))

  in
    iterate 0


cappedGenerateUntil : Int -> (a -> Bool) -> Generator a -> Seed -> List a
cappedGenerateUntil maxGenerations predicate generator seed =
  if maxGenerations <= 0
  then
    []
  else
    let (value, nextSeed) = generate generator seed
    in
      if predicate value
      then
        value :: cappedGenerateUntil (maxGenerations - 1) predicate generator nextSeed
      else
        []

quickGenerate : Generator a -> Seed -> a
quickGenerate generator seed =
  (fst (generate generator seed))

mapConstraint : (a -> b) -> Generator a -> Generator (a, b)
mapConstraint constraint generator =
  customGenerator
    (\seed ->
        let (value, seed1) = generate generator seed
        in
          ((value, constraint value), seed1))
