{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module HW09 where

import Ring
import System.Random
import Test.QuickCheck
import Test.HUnit

-- Some Notes About QuickCheck
-- ===========================
--
-- Properties:
--
--  * Names begin with "prop_";
--
--  * Universally quantified over params (i.e. must hold for all instances in
--  param's type domain);
--
--  * Must be monomorphic. Tip: to restrict their signature:
--    > prop_a x y = ...
--    >   where types = (x :: Int, y :: Double)
--
--  * Unless defined by a combinator, must return Bool.
--
--
-- Condiitional Properties (implications):
--
--  * Take the form: <condition> ==> <prop>;
--
--  * Hold if <condition> is false or <prop> is True.
--
--  * Note: Can slow down testing (depending on how many generated test cases
--  are discarded because they not satisfy <condition>).
--
--
-- Gen a:
--
--  * Test data generator for values of type a;
--
--  * built on top of choose, which randomly select a value in a range with
--  uniform distribution:
--    > choose :: Random a => (a, a) -> Gen a
--
--  * Choose between combinators with oneof or frequency:
--    > oneof :: [Gen a] -> Gen a
--    > frequency :: [(Int, Gen a)] -> Gen a
--
--  * Carry an implicit size parameter. To obtain the current size parameter:
--    > sized :: (Int -> Gen a) -> Gen a
--
--
-- Arbitrary:
--
--  * Type class that defines a default test data generator for each type
--  (obtained by invoking method "arbitrary");
--
-- Combinators: what is a combinator?
--

{- Exercise 1 -}

instance Arbitrary Mod5 where
  arbitrary = MkMod <$> choose (0, 4)

instance Arbitrary Mat2x2 where
  arbitrary =
    MkMat
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
  shrink = shrinkMat2x2

{- Exercise 2 -}

-- | Implement shrinking for @Mat2x2@.
--
-- This function tries to gradually bring the matrix to shrink towards the zero
-- matrix, including the identity matrix in the skinked list.
shrinkMat2x2 :: Mat2x2 -> [Mat2x2]
shrinkMat2x2 m = case m of
  _ | m == zero -> []
  _ | m == mid -> [zero]
  (MkMat a00 a01 a10 a11) ->
    let sm0 = MkMat 0 a01 a10 a11
        sm1 = MkMat a00 0 a10 a11
        sm2 = MkMat a00 a01 0 a11
        sm3 = MkMat a00 a01 a10 0
        sms = filter (/= m) [sm0, sm1, sm2, sm3]
     in (zero : mid : sms) <> (sms >>= shrinkMat2x2)
  where
    zero = MkMat 0 0 0 0
    mid = MkMat 1 0 0 1

{- Exercise 3 -}

-- | Check the sum is associative.
prop_1 :: (Eq r, Ring r) => r -> r -> r -> Bool
prop_1 a b c = (a `add` b) `add` c == a `add` (b `add` c)

-- | Check the sum is commutative.
prop_2 :: (Eq r, Ring r) => r -> r -> Bool
prop_2 a b = a `add` b == b `add` a

-- | Check @addId@ is the /additive identity/.
prop_3 :: (Eq r, Ring r) => r -> Bool
prop_3 a = a `add` addId == a

-- | Check the /additive inverse/ exists.
prop_4 :: (Eq r, Ring r) => r -> Bool
prop_4 a = a `add` addInv a == addId

-- | Check the multiplication is associative.
prop_5 :: (Eq r, Ring r) => r -> r -> r -> Bool
prop_5 a b c = (a `mul` b) `mul` c == a `mul` (b `mul` c)

-- | Check the left /multiplicative identity/.
prop_6 :: (Eq r, Ring r) => r -> Bool
prop_6 a = mulId `mul` a == a

-- | Check the right /multiplicative identity/.
prop_7 :: (Eq r, Ring r) => r -> Bool
prop_7 a = a `mul` mulId == a

-- | Check the multiplication is left-distributive w.r.t. the sum.
prop_8 :: (Eq r, Ring r) => r -> r -> r -> Bool
prop_8 a b c = a `mul` (b `add` c) == (a `mul` b) `add` (a `mul` c)

-- | Check the multiplication is right-distributive w.r.t. the sum.
prop_9 :: (Eq r, Ring r) => r -> r -> r -> Bool
prop_9 a b c = (b `add` c) `mul` a == (b `mul` a) `add` (c `mul` a)

{- Exercise 4 -}

-- | Check all of the properties above on class type @Ring@.
prop_ring :: (Eq r, Ring r) => r -> r -> r -> Property
prop_ring a b c =
  conjoin
    [ counterexample "sum associative" $ prop_1 a b c,
      counterexample "sum commutative" $ prop_2 a b,
      counterexample "additive identity" $ prop_3 a,
      counterexample "additive inverse" $ prop_4 a,
      counterexample "mul associative" $ prop_5 a b c,
      counterexample "left mul identity" $ prop_6 a,
      counterexample "right mul identity" $ prop_7 a,
      counterexample "left distributivity" $ prop_8 a b c,
      counterexample "right distributivity" $ prop_9 a b c
    ]

{- Exercise 5 -}

-- | Check the ring properties for all the Ring instances that have been
-- defined so far.
--
-- The broken type instance is @Ring Bool@.
checkAllRings :: IO ()
checkAllRings =
  do
    quickCheck $ counterexample "== Ring Mat2x2" $ prop_ring . forMat2x2
    quickCheck $ counterexample "== Ring Mod5" $ prop_ring . forMod5
    quickCheck $ counterexample "== Ring Integer" $ prop_ring . forInteger
    quickCheck $ counterexample "== Ring Bool" $ prop_ring . forBool
  where
    -- Despicable tricks to force type of properties.
    forInteger (x :: Integer) = x
    forMat2x2 (x :: Mat2x2) = x
    forMod5 (x :: Mod5) = x
    forBool (x :: Bool) = x

{- Exercise 6 -}

-- | A binary search tree.
data BST a
  = Leaf
  | Node (BST a) a (BST a)
  deriving (Show)

-- | Is the tree a BST between the given endpoints?
isBSTBetween ::
  Ord a =>
  -- | lower bound, if one exists
  Maybe a ->
  -- | upper bound, if one exists
  Maybe a ->
  -- | tree to test
  BST a ->
  Bool
isBSTBetween _ _ Leaf = True
isBSTBetween m_lower m_upper (Node left x right) =
  maybe True (<= x) m_lower
    && maybe True (x <=) m_upper
    && isBSTBetween m_lower (Just x) left
    && isBSTBetween (Just x) m_upper right

-- | Is this a valid BST?
isBST :: Ord a => BST a -> Bool
isBST = isBSTBetween Nothing Nothing

-- | Is this a leaf
isLeaf :: BST a -> Bool
isLeaf Leaf = True
isLeaf _ = False

{- Exercise 7 -}

-- NOTE: Do I really need both Random and Arbitrary?
instance (Arbitrary a, Random a, Ord a) => Arbitrary (BST a) where
  arbitrary =
    -- NOTE: Applicative interface not enough. As gen returns Gen a, passing it
    -- through "ap" returns Gen (Gen a). To prevent this, I need bind or join.
    -- Both belong to the Monad interface.
    --
    -- join $ gen <$> arbitrary <*> arbitrary
    -- where
    --   gen x y =
    --     let l = min x y
    --         u = max x y
    --      in genBST l u

    let gen x y =
          let l = min x y
              u = max x y
           in genBST l u
     in
        do
          x <- arbitrary
          y <- arbitrary
          gen x y


-- | Helper function: create a random generator for binary search trees.
genBST ::
  (Random a, Ord a) =>
  -- | The lower bound
  a ->
  -- | The upper bound
  a ->
  Gen (BST a)
genBST l u =
  let genSubtree =
        choose (l, u)
          >>= \x ->
            Node
              <$> genBST l x
              <*> pure x
              <*> genBST x u
   in oneof
        [ return Leaf, -- Note: 50% chance of getting leaf at each recursion.
          genSubtree
        ]

-- | Check that my implementation of Arbitrary for BST a always returns valid
-- binary search trees.
prop_isBST :: (Ord a) => BST a -> Bool
prop_isBST = isBST

{- Exercise 8 -}

-- | Helper function: create a random generator for binary search trees.
genBST' ::
  (Random a, Ord a) =>
  -- | The lower bound
  a ->
  -- | The upper bound
  a ->
  -- | The size
  Int ->
  Gen (BST a)
genBST' l u s =
  let genSubtree =
        choose (l, u)
          >>= \x ->
            Node
              <$> genBST' l x (s `div` 2)
              <*> pure x
              <*> genBST' x u (s `div` 2)
   in frequency [(1, return Leaf), (s, genSubtree)]

{- Exercise 9 -}

testIntegerParser :: Test
testIntegerParser =
  let nothing = Nothing :: Maybe Integer
      i (x :: Integer) = x
   in TestList
        [ "empty" ~: parseAll "" ~?= nothing,
          -- Parser can handle leading spaces.
          "leading space" ~: parseAll " 0" ~?= Just (i 0),
          -- Parser cannot handle leading alphabetic chars.
          "leading alpha char" ~: parseAll "a0" ~?= nothing,
          "trailing alpha char" ~: parse "0a" ~?= Just (i 0, "a"),
          "trailing space" ~: parse "0 " ~?= Just (i 0, " "),
          "positive parse" ~: parseAll "7" ~?= Just (i 7),
          "negative parse" ~: parseAll "-1" ~?= Just (i (-1))
        ]

testMod5Parser :: Test
testMod5Parser =
  let nothing = Nothing :: Maybe Mod5
      m5 = MkMod
   in TestList
        [ "empty" ~: parseAll "" ~?= nothing,
          -- Parser can handle leading spaces.
          "leading space" ~: parseAll " 0" ~?= Just (m5 0),
          -- Parser cannot handle leading alphabetic chars.
          "leading alpha char" ~: parseAll "a0" ~?= nothing,
          "trailing alpha char" ~: parse "0a" ~?= Just (m5 0, "a"),
          "trailing space" ~: parse "0 " ~?= Just (m5 0, " "),
          "positive wrap" ~: parseAll "7" ~?= Just (m5 2),
          "negative wrap" ~: parseAll "-1" ~?= Just (m5 4)
        ]

testBoolParser :: Test
testBoolParser =
  let nb = Nothing :: Maybe Bool
      nbs = Nothing :: Maybe( Bool, String)
   in TestList
        [ "empty" ~: parseAll "" ~?= nb,
          "'T'" ~: parseAll "True" ~?= Just True,
          "'F'" ~: parseAll "False" ~?= Just False,

          -- Parser can handle leading spaces.
          "'T' leading space" ~: parseAll " True" ~?= Just True,
          "'F' leading space" ~: parseAll " False" ~?= Just False,

          -- Parser cannot handle leading alphabetic chars.
          "'T' leading alpha char" ~: parseAll "aTrue"  ~?= nb,
          "'F' leading alpha char" ~: parseAll "aFalse" ~?= nb,

          -- Parser can handle trailing spaces.
          "'T' trailing space" ~: parse "True " ~?= Just (True, " "),
          "'F' trailing space" ~: parse "False " ~?= Just (False, " "),

          -- Parser cannot handle trailing alphabetic chars.
          "'T' trailing alpha char" ~: parse "Truea"  ~?= nbs,
          "'F' trailing alpha char" ~: parse "Falsea" ~?= nbs,

          "'yes' parse" ~: parseAll "yes" ~?= nb,
          "'no' parse"  ~: parseAll "no"  ~?= nb,

          "positive int parse" ~: parseAll "1"  ~?= nb,
          "zero int parse"     ~: parseAll "0"  ~?= nb,
          "negative int parse" ~: parseAll "-1" ~?= nb
        ]

testMat2x2Parser :: Test
testMat2x2Parser =
  let n = Nothing :: Maybe Mat2x2
      z = MkMat 0 0 0 0
      mid = MkMat 1 0 0 1
   in TestList
        [ "empty" ~: parseAll "" ~?= n,
          "Valid zero" ~: parseAll "[[0,0][0,0]]" ~?= Just z,
          "Valid identity" ~: parseAll "[[1,0][0,1]]" ~?= Just mid,
          -- Parser doesn't handle leading spaces
          "leading space" ~: parseAll " [[0,0][0,0]]" ~?= n,
          -- Parser handles leading spaces in front of ints
          "spaces before ints" ~: parseAll "[[ 0, 0][ 0, 0]]" ~?= Just z,
          -- Parser handles trailing spaces after ints
          "spaces after ints" ~: parseAll "[[0 ,0][0,0]]" ~?= n,
          "spaces among first brackets" ~: parseAll "[ [0,0][0,0]]" ~?= n,
          "spaces among second brackets" ~: parseAll "[[0,0] [0,0]]" ~?= n,
          "spaces among third brackets" ~: parseAll "[[0,0][0,0] ]" ~?= n,
          -- Parser handles trailing spaces
          "trailing space" ~: parse "[[0,0][0,0]] " ~?= Just (z, " "),
          "floats" ~: parseAll "[[0.,0][0,0]] " ~?= n
        ]

-- | Parse all the @Parsable@ instances in Ring.hs
parserTests :: Test
parserTests =
  TestList
    [ "Parsable Integer" ~: testIntegerParser,
      "Parsable Mod5" ~: testMod5Parser,
      "Parsable Bool" ~: testBoolParser,
      "Parsable Mat2x2" ~: testMat2x2Parser
    ]
