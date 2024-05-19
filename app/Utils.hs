module Utils(mapWithState, flipMap, takeWhilePlus1) where
import qualified Data.Map as Map
import Data.Tuple

{-|
Transforms each element (like `map`), but a state is passed from one call to the next one.

>>> mapWithState (\x state -> (x+state, state+1)) 0 [0..4]
([0,2,4,6,8],5)

>>> take 5 $ fst $ mapWithState (\x st -> (x+st, st+1)) 0 [0..] -- works on infinite lists if the final state is ignored
[0,2,4,6,8]
-}
mapWithState :: (a -> state -> (b, state)) -> state -> [a] -> ([b], state)
mapWithState _ state [] = ([], state)
mapWithState f state (x:xs) =
  let (z, state2) = f x state
      (zs, finalState) = mapWithState f state2 xs
  in (z:zs, finalState)

-- | Values are now keys and vice versa.
flipMap :: Ord v => Map.Map k v -> Map.Map v k
flipMap m = Map.fromList $ map swap (Map.toList m)

{-|
Similar to `takeWhile`, but the first element, on which the predicate failed, is also included.

>>> takeWhilePlus1 (== 1) [1,1,1,1,0,0,0,0]
[1,1,1,1,0]
-}
takeWhilePlus1 :: (a -> Bool) -> [a] -> [a]
takeWhilePlus1 _ [] = []
takeWhilePlus1 p (x:xs)
  | p x       = x : takeWhilePlus1 p xs
  | otherwise = [x]
