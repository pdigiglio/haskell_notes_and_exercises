{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Data.List
import Data.Monoid
import Control.Monad.Random

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield
  { --
    attackers :: Army,
    defenders :: Army
  }
  deriving (Show)

{- Exercise 2 -}

-- | Get the maximum defending units in a battlefield.
maxDefendingUnits :: Battlefield -> Army
maxDefendingUnits = min 2 . defenders 

-- | Get the maximum attacking units in a battlefield.
maxAttackingUnits :: Battlefield -> Army
maxAttackingUnits bf = min 3 $ attackers bf - 1 

-- | Count the number of times a predicate returns @True@ in a @Foldable@. 
countIf :: Foldable t => (a -> Bool) -> t a -> Int
countIf p = getSum . foldMap (Sum . fromEnum . p)

-- | Sort in reverse order.
revSort :: Ord a => [a] -> [a]
revSort = reverse . sort

-- | Update a battlefield after rolling dice.
updateBattlefield ::
  -- | The battlefield
  Battlefield ->
  -- | The attacker's die results
  [DieValue] ->
  -- | The defender's die results.
  [DieValue] ->
  Battlefield
updateBattlefield bf att def =
  let matchup = zipWith (-) (revSort att) (revSort def)
      defLoss = countIf (> 0) matchup
      attLoss = length matchup - defLoss
   in Battlefield
        { attackers = attackers bf - attLoss,
          defenders = defenders bf - defLoss
        }

-- | Simulate a single battle.
battle :: Battlefield -> Rand StdGen Battlefield
battle bf =
  let attUnits = maxAttackingUnits bf
      defUnits = maxDefendingUnits bf
   in replicateM attUnits die >>= \att ->
        replicateM defUnits die >>= \def ->
          return $ updateBattlefield bf att def

--battle bf =
--  let attUnits = maxAttackingUnits bf
--      defUnits = maxDefendingUnits bf
--   in do
--        att <- replicateM attUnits die
--        def <- replicateM defUnits die
--        return $ updateBattlefield bf att def


{- Exercise 3 -}

iterateWhileM :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m a
iterateWhileM p f x
  | p x = f x >>= iterateWhileM p f
  | otherwise = return x

canStillBattle :: Battlefield -> Bool
canStillBattle bf =
  let canAttack = (> 0) . maxAttackingUnits
      canDefend = (> 0) . maxDefendingUnits
   in canAttack bf && canDefend bf

invade :: Battlefield -> Rand StdGen Battlefield
invade = iterateWhileM canStillBattle battle 

{- Exercise 4 -}

-- | Return @True@ if the attacker wins (i.e. the defender has no army).
--attackerWins :: Battlefield -> Bool
--attackerWins bf = defenders bf == 0

-- | Estimate the probability of an attacker successfully invading on a given
-- battlefield.
--
-- The estimation is performed by running 1000 simulated invasions and measuring
-- the attacker's success rate.
successProb :: Battlefield -> Rand StdGen Double
successProb =
  let n = 1000
      attackerWins = (== 0) . defenders
   in fmap ((/ fromIntegral n) . fromIntegral . countIf attackerWins)
        . replicateM n
        . invade
