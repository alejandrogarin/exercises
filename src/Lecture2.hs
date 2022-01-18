{- |
Module                  : Lecture2
Copyright               : (c) 2021-2022 Haskell Beginners 2022 Course
SPDX-License-Identifier : MPL-2.0
Maintainer              : Haskell Beginners 2022 Course <haskell.beginners2022@gmail.com>
Stability               : Stable
Portability             : Portable

Exercises for the Lecture 2 of the Haskell Beginners course.

As in the previous section, implement functions and provide type
signatures. If the type signature is not already written, write the
most polymorphic type signature you can.

Unlike exercises to Lecture 1, this module also contains more
challenging exercises. You don't need to solve them to finish the
course but you can if you like challenges :)
-}

module Lecture2
    ( -- * Normal
      lazyProduct
    , duplicate
    , removeAt
    , evenLists
    , dropSpaces

    , Knight (..)
    , Dragon (..)
    , EvilDragon (..)
    , FightResult (..)
    , Reward (..)
    , Chest (..)
    , Treasure (..)
    , XP (..)
    , Gold (..)
    , Health (..)
    , Attack (..)
    , FirePower (..)
    , Endurance (..)
    , dragonFight

      -- * Hard
    , isIncreasing
    , merge
    , mergeSort

    , Expr (..)
    , Variables
    , EvalError (..)
    , eval
    , constantFolding
    ) where
import Data.Char (isSpace)

{- | Implement a function that finds a product of all the numbers in
the list. But implement a lazier version of this function: if you see
zero, you can stop calculating product and return 0 immediately.

>>> lazyProduct [4, 3, 7]
84
-}
lazyProduct :: [Int] -> Int
lazyProduct list = go list 1
  where go :: [Int] -> Int -> Int
        go [] acc      = acc
        go (0: _) _    = 0
        go (x: xs) acc = go xs (acc * x)

{- | Implement a function that duplicates every element in the list.

>>> duplicate [3, 1, 2]
[3,3,1,1,2,2]
>>> duplicate "cab"
"ccaabb"
-}
duplicate :: [a] -> [a]
duplicate list = go list []
  where go [] acc      = acc
        go (x: xs) acc = x : x : go xs acc

{- | Implement function that takes index and a list and removes the
element at the given position. Additionally, this function should also
return the removed element.

>>> removeAt 0 [1 .. 5]
(Just 1,[2,3,4,5])

>>> removeAt 10 [1 .. 5]
(Nothing,[1,2,3,4,5])
-}
removeAt :: Int -> [a] -> (Maybe a, [a])
removeAt index list =
  if index < 0 || index >= length list
  then (Nothing, list)
  else  go 0 list []
  where go :: Int -> [a] -> [a] -> (Maybe a, [a])
        go _ [] acc = (Nothing, acc)
        go currentIndex (x : xs) acc =
          if currentIndex == index
          then (Just x, reverse acc ++ xs)
          else go (currentIndex + 1) xs (x : acc)

{- | Write a function that takes a list of lists and returns only
lists of even lengths.

>>> evenLists [[1,2,3], [3,1,2,7], [], [5, 7, 2]]
[[3,1,2,7],[]]

♫ NOTE: Use eta-reduction and function composition (the dot (.) operator)
  in this function.
-}
evenLists :: [[a]] -> [[a]]
evenLists = filter (even . length)

{- | The @dropSpaces@ function takes a string containing a single word
or number surrounded by spaces and removes all leading and trailing
spaces.

>>> dropSpaces "   hello  "
"hello"
>>> dropSpaces "-200            "
"-200"

♫ NOTE: As in the previous task, use eta-reduction and function
  composition (the dot (.) operator) in this function.

🕯 HINT: look into Data.Char and Prelude modules for functions you may use.
-}
dropSpaces :: String -> String
dropSpaces = filter (not . isSpace)

{- |

The next task requires to create several data types and functions to
model the given situation.

An evil dragon attacked a village of innocent citizens! After
returning to its lair, the dragon became hungry and ate one of its
treasure chests by accident.

The guild in the village found a brave knight to slay the dragon!
As a reward, the knight can take the treasure chest.

Below is the description of the fight and character specifications:

  * A chest contains a non-zero amount of gold and a possible treasure.
  * As a reward, knight takes all the gold, the treasure and experience.
  * Experience is calculated based on the dragon type. A dragon can be
    either red, black or green.
  * Red dragons grant 100 experience points, black dragons — 150, and green — 250.
  * Stomachs of green dragons contain extreme acid and they melt any
    treasure except gold. So green dragons has only gold as reward.
    All other dragons always contain treasure in addition to gold.
  * Knight tries to slay dragon with their sword. Each sword strike
    decreases dragon health by the "sword attack" amount. When the
    dragon health becomes zero or less, a dragon dies and the knight
    takes the reward.
  * After each 10 sword strikes, dragon breathes fire and decreases
    knight health by the amount of "dragon fire power". If the
    knight's health becomes 0 or less, the knight dies.
  * Additionally, each sword strike decreases "knight's endurance" by one.
    If knight's endurance becomes zero, they become tired and are not
    able to continue the fight so they run away.

Implement data types to describe treasure, knight and dragon.
And implement a function that takes a knight and a dragon and returns
one of the three possible fight outcomes.

You're free to define any helper functions.

🕯 HINT: If you find the description overwhelming to implement entirely
  from scratch, try modelling the problem in stages.

    1. Implement all custom data types without using polymorphism.
    2. Add @newtype@s for safety where you think is appropriate.
    3. Encode the fight result as a sum type.
    4. Add polymorphism.
    5. Make all invalid states unrepresentable. Think, how you can
       change your types to prevent green dragons from having any
       treasure besides gold (if you already haven't done this).
-}

-- some help in the beginning ;)
data Knight = Knight
    { knightHealth    :: Health
    , knightAttack    :: Attack
    , knightEndurance :: Endurance
    } deriving (Eq)

data Dragon = Dragon
    { dragonHealth    :: Health
    , dragonFirePower :: FirePower
    } deriving (Eq)

data Chest a = Chest
    { gold      :: Gold
    , threasure :: Maybe a
    } deriving (Show, Eq)

data Reward = Reward Gold (Maybe Treasure) XP deriving (Show, Eq)

data Treasure = Gems | Jewels deriving (Show, Eq)

data EvilDragon = Green Dragon Gold
                | Black Dragon (Chest Treasure)
                | Red   Dragon (Chest Treasure) deriving (Eq)

data FightResult = KnightWin Reward | KnightDie | KnightRunAway deriving (Show, Eq)

newtype Gold = Gold Int deriving (Show, Eq)

newtype Health = Health Int deriving (Show, Eq)

newtype Attack = Attack Int deriving (Show, Eq)

newtype Endurance = Endurance Int deriving (Show, Eq)

newtype FirePower = FirePower Int deriving (Show, Eq)

newtype XP = XP Int deriving (Show, Eq)

newtype Error = Error String deriving (Show, Eq)

dragonFight :: Knight -> EvilDragon -> Either Error FightResult
dragonFight knight evilDragon = evaluateResult (snd fightResult) (fst fightResult)
    where go :: Int -> Knight -> EvilDragon -> (Knight, EvilDragon)
          go strikeNumber knight' dragon
            | isFightFinished evilDragon knight' = (knight', dragon)
            | strikeNumber > maxStrikesInRound   = go 1 (damageKnight dragon knight') dragon
            | otherwise                          = go (strikeNumber + 1) (reduceEndurance knight') (damageDragon knight' dragon)
          fightResult :: (Knight, EvilDragon)
          fightResult = go 1 knight evilDragon

maxStrikesInRound :: Int
maxStrikesInRound = 10

evaluateResult :: EvilDragon -> Knight -> Either Error FightResult
evaluateResult evilDragon knight
  | isDead (dragonHealth dragon) = Right $ KnightWin $ createReward evilDragon
  | isEnduranceEmpty (knightEndurance knight) = Right KnightRunAway
  | isDead (knightHealth knight) = Right KnightDie
  | otherwise = Left $ Error "something went wrong :("
  where dragon = extractDragon evilDragon

isFightFinished :: EvilDragon -> Knight -> Bool
isFightFinished dragon knight = isDead (knightHealth knight)
                             || isDead (dragonHealth (extractDragon dragon))
                             || isEnduranceEmpty (knightEndurance knight)

isDead :: Health -> Bool
isDead (Health h) = h <= 0

damageKnight :: EvilDragon -> Knight -> Knight
damageKnight dragon knight = knight {knightHealth = reduceHealthByFirePower (knightHealth knight) (dragonFirePower (extractDragon dragon))}

damageDragon :: Knight -> EvilDragon -> EvilDragon
damageDragon knight evilDragon = embedDragon (dragon {dragonHealth = reduceHealthByAttack (dragonHealth dragon) (knightAttack knight)}) evilDragon
  where dragon = extractDragon evilDragon

reduceEndurance :: Knight -> Knight
reduceEndurance knight@(Knight _ _ (Endurance e)) = knight {knightEndurance = Endurance (e - 1)}

createReward :: EvilDragon -> Reward
createReward dragon = let chest = extractChest dragon
                          gold' = gold chest
                          threasure' = threasure chest
                          xp = extractXP dragon
                      in Reward gold' threasure' xp

extractDragon :: EvilDragon -> Dragon
extractDragon (Green d _) = d
extractDragon (Black d _) = d
extractDragon (Red d _)   = d

embedDragon :: Dragon -> EvilDragon -> EvilDragon
embedDragon dragon (Green _ gold)  = Green dragon gold
embedDragon dragon (Black _ chest) = Black dragon chest
embedDragon dragon (Red _ chest)   = Red dragon chest

extractChest :: EvilDragon -> Chest Treasure
extractChest (Green _ g) = Chest g Nothing
extractChest (Black _ c) = c
extractChest (Red _ c)   = c

extractXP :: EvilDragon -> XP
extractXP (Green _ _) = XP 250
extractXP (Black _ _) = XP 150
extractXP (Red _ _)   = XP 100

isEnduranceEmpty :: Endurance -> Bool
isEnduranceEmpty (Endurance e) = e <= 0

reduceHealthByFirePower :: Health -> FirePower -> Health
reduceHealthByFirePower (Health h) (FirePower fp) = Health (h - fp)

reduceHealthByAttack :: Health -> Attack -> Health
reduceHealthByAttack (Health h) (Attack a) = Health (h - a)

-- Q1: What is supposed to do to avoid part of this boilerplate functions? Optics?

-- Q2: I see Haskell provides a way to create a copy of your product type by just changing only 1 property (Record syntax)
--     What is supposed to do with sum types? I didn't see a dual version for it

----------------------------------------------------------------------------
-- Challenges
----------------------------------------------------------------------------

{- The following exercises are considered more challenging. However,
you still may find some of them easier than some of the previous
ones. Difficulty is a relative concept.
-}

{- | Write a function that takes a list of numbers and returns 'True'
if all the numbers are in the increasing order (i.e. the list is
sorted).

>>> isIncreasing [3, 1, 2]
False
>>> isIncreasing [1 .. 10]
True
-}
isIncreasing :: [Int] -> Bool
isIncreasing [] = True
isIncreasing (first : rest) = go first rest
  where go :: Int -> [Int] -> Bool
        go _ []       = True
        go n (x : xs) = (n <= x) && go x xs

{- | Implement a function that takes two lists, sorted in the
increasing order, and merges them into new list, also sorted in the
increasing order.

The lists are guaranteed to be given sorted, so you don't need to
verify that.

>>> merge [1, 2, 4] [3, 7]
[1,2,3,4,7]
-}
merge :: [Int] -> [Int] -> [Int]
merge [] ys           = ys
merge xs []           = xs
merge (x: xs) (y: ys) = if x < y
                        then x : merge xs (y: ys)
                        else y : merge (x : xs) ys

{- | Implement the "Merge Sort" algorithm in Haskell. The @mergeSort@
function takes a list of numbers and returns a new list containing the
same numbers but in the increasing order.

The algorithm of merge sort is the following:

  1. If the given list has less than 2 elements, it's already sorted.
  2. Otherwise, split list into two lists of the same size.
  3. Sort each of two lists recursively.
  4. Merge two resulting sorted lists to get a new sorted list.

>>> mergeSort [3, 1, 2]
[1,2,3]
-}
mergeSort :: [Int] -> [Int]
mergeSort = go . inverseConcat
  where
    go :: [[Int]] -> [Int]
    go []               = []
    go [xs]             = xs
    go (xs : ys : rest) = go (merge xs ys : rest)

inverseConcat :: [Int] -> [[Int]]
inverseConcat = go
  where go :: [Int] -> [[Int]]
        go []       = []
        go (x : xs) = [x] : go xs

{- | Haskell is famous for being a superb language for implementing
compilers and interpeters to other programming languages. In the next
tasks, you need to implement a tiny part of a compiler.

We're going to work on a small subset of arithmetic operations.

In programming we write expressions like "x + 1" or "y + x + 10".
Such expressions can be represented in a more structured way (than a
string) using the following recursive Algebraic Data Type:
-}
data Expr
    = Lit Int
    | Var String
    | Add Expr Expr
    deriving (Show, Eq)

{- Now, you can use this data type to describe such expressions:

> x + 1
Add (Var "x") (Lit 1)

> y + x + 10
Add (Var "y") (Add (Var "x") (Lit 10))
-}

{- | We want to evaluate such expressions. We can associate a value
with a variable using a list of pairs.

You can use the @lookup@ function to search in this list by a variable name:

 * https://hackage.haskell.org/package/base-4.16.0.0/docs/Prelude.html#v:lookup
-}
type Variables = [(String, Int)]

{- | Unfortunately, it's not guaranteed that variables in our @Expr@
data type are present in the given list. So we're going to introduce a
separate data for possible evaluation errors.

Normally, this would be a sum type with several constructors
describing all possible errors. But we have only one error in our
evaluation process.
-}
data EvalError
    = VariableNotFound String
    deriving (Show, Eq)

{- | Having all this set up, we can finally implement an evaluation function.
It returns either a successful evaluation result or an error.
-}
eval :: Variables -> Expr -> Either EvalError Int
eval _ (Lit value)    = Right value
eval vars (Var var)   = case lookup var vars of
                          Just value -> Right value
                          Nothing    -> Left $ VariableNotFound var
eval vars (Add e1 e2) = case (eval vars e1, eval vars e2) of
                          (Right value1, Right value2) -> Right $ value1 + value2
                          (Left e, _)                  -> Left e
                          (_, Left e)                  -> Left e

{- | Compilers also perform optimizations! One of the most common
optimizations is "Constant Folding". It performs arithmetic operations
on all constants known during compile time. This way you can write
more verbose and clear code that works as efficient as its shorter
version.

For example, if you have an expression:

x + 10 + y + 15 + 20

The result of constant folding can be:

x + y + 45

It also can be:

x + 45 + y

Write a function that takes and expression and performs "Constant
Folding" optimization on the given expression.
-}

-- 😱 😱 😱
constantFolding :: Expr -> Expr
constantFolding lit@(Lit _)              = lit
constantFolding val@(Var _)              = val
constantFolding (Add (Lit v1) (Lit v2))  = Lit $ v1 + v2
constantFolding (Add (Var v) (Lit l))    = constantFolding (Add (Lit l) (Var v))
constantFolding (Add (Lit l) (Var v))    = if l > 0 then Add (Lit l) (Var v) else Var v
constantFolding vv@(Add (Var _) (Var _)) = vv
constantFolding (Add (Lit _) e2) = constantFolding e2
constantFolding (Add e1 (Lit _)) = constantFolding e1
constantFolding (Add (Add (Var v1) (Lit l1)) (Add (Var v2) (Lit l2))) = constantFolding (Add (Add (Lit l1) (Var v1)) (Add (Lit l2) (Var v2)))
constantFolding expr@(Add (Add (Lit _) (Var v)) (Add (Lit _) (Var v1))) = Add (Lit (foldLit expr 0)) (constantFolding (Add (Var v) (Var v1)))
constantFolding expr = Add (Lit (foldLit expr 0)) expr

foldLit :: Expr -> Int -> Int
foldLit (Lit value) litAcc = litAcc + value
foldLit (Var _) litAcc     = litAcc
foldLit (Add e1 e2) litAcc = foldLit e1 litAcc + foldLit e2 litAcc
