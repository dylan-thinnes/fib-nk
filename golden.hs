{-# LANGUAGE OverloadedLists #-}
import Prelude hiding (cycle)
import qualified Prelude (cycle)
import qualified Data.Set as S
import Data.Maybe (fromMaybe)
import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq(..), (><))
import System.Environment

type Seed = [Int]
type Step = Seed -> Seed
newtype Cycle = Cycle { uncycle :: Seq Seed }
data Group = Group { k :: Int
                   , n :: Int
                   , ungroup :: [Cycle]
                   }
type Domain = S.Set Seed

main = do
    [arg0, arg1] <- getArgs
    let k = read arg0
    let n = read arg1
    let group = fibnk n k
    putStr $ debugGroup group

-- FIB-SPECIFIC

incr :: Seed -> Seed
incr xs = sum xs : init xs

incrMod :: Int -> Seed -> Seed
incrMod n ys = let (x:xs) = incr ys in mod x n : xs

domain :: Int -> Int -> Domain
domain n k = S.fromList $ f n k
    where
    f :: Int -> Int -> [Seed]
    f n 1 = pure <$> [0..n-1]
    f n k = (:) <$> [0..n-1] <*> f n (k-1)

-- Finds all cycles for a specific fibonacci progression
fibnk :: Int -> Int -> Group
fibnk n k = Group k n $ cycles (incrMod n) $ domain n k

-- GENERAL

-- Find the cycle of an injective function on a finite domain, given a seed val
cycle :: Step -> Seed -> Cycle
cycle step seed = Cycle $ Seq.fromList 
                $ seed : takeWhile (/= seed) (iterate step (step seed))

-- Find all the cycles of an injective function on a finite domain, given a set
-- for the domain
cycles :: Step -> Domain -> [Cycle]
cycles step domain = f domain []
    where
    f :: Domain -> [Cycle] -> [Cycle]
    f domain found 
      | S.null domain 
      = found
      | otherwise     
      = let newCycle = cycle step (S.findMin domain)
         in f (foldr S.delete domain $ uncycle newCycle) (newCycle : found)

-- Pretty printing of cycles and groups

instance Show Cycle where
    show (Cycle (_ :<| c)) 
      = unwords $ toList $ fmap (show . head) c

instance Show Group where
    show (Group _ _ cycles) = unlines $ map f $ zip [0..] cycles
        where
        f :: (Int, Cycle) -> String
        f (n,c) = "Cycle #" ++ show n ++ ":\n " ++ show c
                  ++ "\n  length: " ++ show (length $ uncycle c)

-- Show a group in the simplest way possible, one line per cycle, one word per
-- element
-- Make sure that the end is padded w/ values from the beginning, so that all
-- states can be reconstructed in a single pass
debugGroup :: Group -> String
debugGroup (Group n k cycles) = unlines $ map debugCycle $ cycles
    where
    debugCycle cycle 
      = let states = uncycle cycle
            values = fmap head states
         in unwords $ map show $ toList
          $ values >< Seq.take (n-1) (Seq.cycleTaking (n-1) values)
