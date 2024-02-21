{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Data.Tree
import Data.Tuple
import Employee

{- Exercise 1 -}

-- | An empty @GuestList@.
glEmpty :: GuestList
glEmpty = GL [] 0

-- | Create a @GuestList@ from an @Employee@.
--
-- The guest list only contains the given employee.
singletonGL :: Employee -> GuestList
singletonGL e = GL [e] (empFun e)

-- | Add an employee to a guest list.
--
-- This function assumes that:
--
--  * The employee is not already in the list.
--  * Nor his direct boss nor any of his direct subordinates are in the list.
--
-- Under these assumptions, I can:
--
--  * Add the employee to the list.
--  * Add the employee fun to the total fun.
--
-- These assumptions hold as long as each employee that gets added to the list
-- has the same direct boss.
glCons :: Employee -> GuestList -> GuestList
glCons e (GL es f) = GL (e : es) (empFun e + f)

-- | Join two @GuestList@s.
--
-- Calling @l +++ r@ is effectively the same as calling @glCons e r@ for each
-- @e@ in the @Employee@ list of @l@. So the prerequisites for @glCons@ must be
-- satisfied.
(+++) :: GuestList -> GuestList -> GuestList
(GL les lf) +++ (GL res rf) = GL (les ++ res) (lf + rf)

-- | A type to make @(GuestList, (+++), glEmpty)@ a monoid.
newtype GLCons = GLCons GuestList

-- | Extract the @GuestList@ from @GLCons@.
getGLCons :: GLCons -> GuestList
getGLCons (GLCons gl) = gl

instance Semigroup GLCons where
  (GLCons l) <> (GLCons r) = GLCons $ l +++ r

instance Monoid GLCons where
  mempty = GLCons glEmpty

-- | Return the guest list with the most fun.
moreFun :: GuestList -> GuestList -> GuestList
moreFun l@(GL _ lf) r@(GL _ rf) = if lf > rf then l else r

-- | A type to make @(GuestList, moreFun, glEmpty)@ a monoid.
newtype MoreFun = MoreFun GuestList

-- | Extract the @GuestList@ from @MoreFun@.
getMoreFun :: MoreFun -> GuestList
getMoreFun (MoreFun gl) = gl

instance Semigroup MoreFun where
  (MoreFun l) <> (MoreFun r) = MoreFun $ moreFun l r

instance Monoid MoreFun where
  mempty = MoreFun glEmpty

{- Exercise 2 -}

-- | Fold a tree.
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node rl sf) = f rl $ map (treeFold f) sf

{- Exercise 3 -}

-- | Apply a function to both elements of a pair.
(+>) :: (a -> b) -> (a, a) -> (b, b)
f +> (x, y) = (f x, f y)

-- | Return the best @GuestList@s for the current subtree (i.e. the subtree
-- rooted at and including the boss).
--
-- The first returned list contains the direct subordinates of the boss, the
-- second does not.
--
-- In this implementation, I also try to handle the case where someone has a
-- negative amount of fun: just don't invite him.
nextLevel ::
  -- | The boss of the curren subtree.
  Employee ->
  -- | The best guest lists for this division so far (ecluding the boss).
  [(GuestList, GuestList)] ->
  -- | The best guest lists (including the boss).
  (GuestList, GuestList)
nextLevel boss =
  let b = if empFun boss < 0 then glEmpty else singletonGL boss
   in swap . (cons +>) . unzip . ((glEmpty, b) :)
  where
    cons = getGLCons . mconcat . map GLCons

{- Exercise 4 -}

-- | Return a @GuestList@ that maximizes the fun for a given company hierarchy.
maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel

{- Exercise 5 -}

-- | Print a guest list in the following format:
--
-- > Total fun: <tot_fun>
-- > <name1>
-- > <name2>
-- > ...
printGuestList :: GuestList -> IO()
printGuestList (GL employees fun) =
  foldl1 (>>)
  . map putStrLn
  . (["Total fun: " ++ show fun] ++)
  . map empName
  $ employees

-- | Read the company hierarchy file the from accompanying file "company.txt",
-- find the fun-maximizing guest list and print it.
main :: IO()
main = readFile "company.txt" >>= (printGuestList . maxFun . read)
