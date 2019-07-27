import Prelude hiding (cycle)
import qualified Prelude (cycle)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Control.Arrow ((&&&))
import System.IO
import System.Environment

type Seed = [Int]
type Step = Seed -> Seed
newtype Cycle = Cycle { uncycle :: [Seed] }
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
    f n 1 = pure <$> [0..n-1]
    f n k = (:) <$> [0..n-1] <*> f n (k-1)

-- Finds all cycles for a specific fibonacci progression
fibnk :: Int -> Int -> Group
fibnk n k = Group k n $ cycles (incrMod n) $ domain n k

-- GENERAL

-- Find the cycle of an injective function on a finite domain, given a seed val
cycle :: Step -> Seed -> Cycle
cycle step seed = Cycle $ seed : takeWhile (/= seed) (iterate step (step seed))

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
    show (Cycle c) = unwords $ map show $ reverse (head c) ++ map head (tail c)

instance Show Group where
    show (Group _ _ cycles) = unlines $ map f $ zip [0..] cycles
        where
        f :: (Int, Cycle) -> String
        f (n,c) = "Cycle #" ++ show n ++ ":\n " ++ show c
                  ++ "\n  length: " ++ show (length $ uncycle c)

cycleLengths :: Group -> M.Map Int Int
cycleLengths (Group _ _ cycles) = foldr f M.empty cycles
    where
    f = M.alter g . length . uncycle
    g = Just . (+1) . fromMaybe 0

-- Show a group in the simplest way possible, one line per cycle, one word per
-- element
-- Make sure that the end is padded w/ values from the beginning, so that all
-- states can be reconstructed in a single pass
debugGroup :: Group -> String
debugGroup (Group n k cycles) = unlines $ map debugCycle $ cycles
    where
    debugCycle cycle 
      = let states = uncycle cycle
            values = map head states
         in unwords $ map show 
                    $ values ++ take (n-1) (Prelude.cycle values)

-- Aggregate reports

report :: [((Int, Int), M.Map Int Int)]
report = map (id &&& (\(n,k) -> cycleLengths $ fibnk n k))
       $ (,) <$> [2..20] <*> [2..5]

reportStr :: String
reportStr = unlines $ showRun <$> report
    where
    showRun ((n,k),lengths)
      = unwords
      $ [show n, show k, ":"
        ,show $ sum $ uncurry (*) <$> M.toList lengths
        ] ++ map show (M.toList lengths)

printReport :: Handle -> IO ()
printReport handle = hPutStr handle reportStr

