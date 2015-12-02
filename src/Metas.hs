{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Metas
( goodTeamRatioArbitaryMeta
, goodTeamRatioTwoToLeftMeta
, GoodTeamRatio(..)
) where

import qualified Data.List as L

import Data.Monoid
import System.Environment

-- Notes:
--     - Round indices are 0-indexed

-- | A seating arrangement. A player is represented as True for good, False for bad
type Seating = [Bool]

data GoodTeamRatio = GoodTeamRatio Int Int deriving (Eq)

instance Show GoodTeamRatio where
    show :: GoodTeamRatio -> String
    show (GoodTeamRatio a b)
        =  "# good teams: " ++ (show a)
        ++ " | # teams: " ++ (show b)
        ++ " | (" ++ (show . roundToKDigits 2 $ ((fromIntegral a) / (fromIntegral b))) ++ ")"
        where
            roundToKDigits :: (Fractional a, RealFrac r) => Int -> r -> a
            roundToKDigits k n = (fromInteger . round $ n * (10^k)) / (10.0^^k)

instance Monoid GoodTeamRatio where
    mempty :: GoodTeamRatio
    mempty = GoodTeamRatio 0 0

    -- | Sum number of good teams and total number of teams respectively
    mappend :: GoodTeamRatio -> GoodTeamRatio -> GoodTeamRatio
    mappend (GoodTeamRatio a b) (GoodTeamRatio c d) = GoodTeamRatio (a + c) (b + d)

-- | /O(2^k)/ Generates all subsets of the given list of size /k/.
subsetsOfSize :: [a] -> Int -> [[a]]
subsetsOfSize l size = subsetsOfSizeRec l [] size
    where
        subsetsOfSizeRec :: [a] -> [a] -> Int -> [[a]]
        subsetsOfSizeRec _   soFar 0 = [soFar]
        subsetsOfSizeRec []  _     _ = []
        subsetsOfSizeRec (s:src') soFar size = without ++ with
            where
                without = subsetsOfSizeRec src' soFar        size
                with    = subsetsOfSizeRec src' (s : soFar) (size - 1)

-- | Utility function on lists -- counts the number of `a`s in the given list
count :: (Eq a) => a -> [a] -> Int
count a = length . filter (== a)

-- | A constant defined by the game
numMissions :: Int
numMissions = 5

-- | A constant defined by the game
numMissionProposals :: Int
numMissionProposals = 5

-- | Given (respectively) the number of players in the game, and which round it is,
-- calculates the mission size. These numbers are constants defined by the game.
getMissionSize :: Int -> Int -> Int
getMissionSize 5 0 = 2
getMissionSize 5 1 = 3
getMissionSize 5 2 = 2
getMissionSize 5 3 = 3
getMissionSize 5 4 = 3
getMissionSize 6 0 = 2
getMissionSize 6 1 = 3
getMissionSize 6 2 = 4
getMissionSize 6 3 = 3
getMissionSize 6 4 = 4
getMissionSize 7 0 = 2
getMissionSize 7 1 = 3
getMissionSize 7 2 = 3
getMissionSize 7 3 = 4
getMissionSize 7 4 = 4
getMissionSize 8 0 = 2
getMissionSize 8 1 = 3
getMissionSize 8 2 = 3
getMissionSize 8 3 = 4
getMissionSize 8 4 = 4
getMissionSize 9 0 = 3
getMissionSize 9 1 = 4
getMissionSize 9 2 = 4
getMissionSize 9 3 = 5
getMissionSize 9 4 = 5
getMissionSize 10 0 = 3
getMissionSize 10 1 = 4
getMissionSize 10 2 = 4
getMissionSize 10 3 = 5
getMissionSize 10 4 = 5
getMissionSize numPlayers roundIndex = error $ "Fatal: illegal (mission size)-round pairing: " ++ (show (numPlayers, roundIndex))

-- | Determines which player starts the mission proposals for round `roundIndex` in a
-- game with `numPlayers` many players
startingKingIndex :: Int -> Int -> Int
startingKingIndex numPlayers roundIndex = roundIndex * numMissions `mod` numPlayers

-- | Under the arbitrary team selection meta, determines whether `teamSelection` made by
-- king `kingIndex` with the seating arrangement `seating` yields a team of all Good
-- (returns False if teamSelection doesn't contain kingIndex)
isGoodTeamArbitraryMeta :: Seating -> Int -> [Int] -> Bool
isGoodTeamArbitraryMeta seating kingIndex teamSelection
    | not (kingIndex `elem` teamSelection) = False
    | otherwise = and . map (seating !!) $ teamSelection

-- | Under the two-to-the-left team selection meta, determines whether the team of size
-- `missionSize` made by king `kingIndex` with the seating arrangement `seating` yields
-- a team of all Good
isGoodTeamTwoToLeftMeta :: Seating -> Int -> Int -> Bool
isGoodTeamTwoToLeftMeta seating missionSize kingIndex
    = and
    . take missionSize
    . drop kingIndex
    $ (seating ++ seating)

--  | Under the two-to-the-left team selection meta, what how many teams are all Good,
-- in round `roundIndex` with seating arrangement `seating` in a game of `numPlayers`
-- many people? Note that we calculate a ratio -- this is because different metas will
-- have different numbers of possible teams, and we are interested in the percentage
-- of good teams selected. Hence, we need the total number of teams considered
goodTeamRatioInRoundTwoToLeftMeta :: Int -> Seating -> Int -> GoodTeamRatio
goodTeamRatioInRoundTwoToLeftMeta numPlayers seating roundIndex
    = (\l -> GoodTeamRatio (count True l) (length l))
    . map (isGoodTeamTwoToLeftMeta seating (getMissionSize numPlayers roundIndex))
    $ [kingIndex .. (kingIndex + numMissionProposals - 1)] -- inclusive => -1
    where
        -- | Which player is the initial king
        kingIndex :: Int
        kingIndex = startingKingIndex numPlayers roundIndex

--  | Under the arbitrary team selection meta, what how many teams are all Good,
-- in round `roundIndex` with seating arrangement `seating` in a game of `numPlayers`
-- many people? Note that we calculate a ratio -- this is because different metas will
-- have different numbers of possible teams, and we are interested in the percentage
-- of good teams selected. Hence, we need the total number of teams considered
goodTeamRatioInRoundArbitaryMeta :: Int -> Seating -> Int -> GoodTeamRatio
goodTeamRatioInRoundArbitaryMeta numPlayers seating roundIndex
    = (\l -> GoodTeamRatio (count True l) (length l))
    $ (isGoodTeamArbitraryMeta seating)
        <$> [kingIndex .. (kingIndex + numMissionProposals - 1)] -- inclusive => -1
        <*> teamSelections
    where
        -- | When evaluating the arbitrary team selection meta, we need to consider all
        -- possible team selections (as opposed to the two-to-the-left meta, in which
        -- there is only one possible team selection each time).
        teamSelections :: [[Int]]
        teamSelections = subsetsOfSize [0 .. (numPlayers - 1)] (getMissionSize numPlayers roundIndex) -- -1 for 0-indexing

        -- | Which player is the initial king
        kingIndex :: Int
        kingIndex = startingKingIndex numPlayers roundIndex

-- | Under the two-to-the-left team selection meta, given seating `seating` in a game
-- with `numPlayers` many players, how many teams are good? Note that we calculate a
-- ratio -- this is because different metas will have different numbers of possible
-- teams, and we are interested in the percentage of good teams selected. Hence, we need
-- the total number of teams considered
goodTeamRatioTwoToLeftMeta :: Int -> Seating -> GoodTeamRatio
goodTeamRatioTwoToLeftMeta numPlayers seating
    = foldl1 (<>)
    . map (goodTeamRatioInRoundTwoToLeftMeta numPlayers seating)
    $ [0 .. (numMissions - 1)]

-- | Under the two-to-the-left team selection meta, given seating `seating` in a game
-- with `numPlayers` many players, how many teams are good? Note that we calculate a
-- ratio -- this is because different metas will have different numbers of possible
-- teams, and we are interested in the percentage of good teams selected. Hence, we need
-- the total number of teams considered
goodTeamRatioArbitaryMeta :: Int -> Seating -> GoodTeamRatio
goodTeamRatioArbitaryMeta numPlayers seating
    = foldl1 (<>)
    . map (goodTeamRatioInRoundArbitaryMeta numPlayers seating)
    $ [0 .. (numMissions - 1)]

test :: IO ()
test = do
    putStrLn "two-to-the-left meta tests"
    putStrLn "--------------------------"

    putStr "number of good teams (two-to-the-left meta) 8 (should be 8): "
    print $ goodTeamRatioTwoToLeftMeta 8 [False, False, False, True, True, True, True, True]

    putStr "number of good teams (two-to-the-left meta) 9 (should be 8): "
    print $ goodTeamRatioTwoToLeftMeta 9 [False, False, False, True, True, True, True, True, True]

    putStr "number of good teams (two-to-the-left meta) 10 (should be 6): "
    print $ goodTeamRatioTwoToLeftMeta 10 [False, False, False, False, True, True, True, True, True, True]

evaluateMetasForNumPlayers :: Int -> IO ()
evaluateMetasForNumPlayers numPlayers = do
    let numBad = case numPlayers of {
        5 -> 2;
        6 -> 2;
        7 -> 3;
        8 -> 3;
        9 -> 3; -- with Mordred
        10 -> 4; }
    let numGood = numPlayers - numBad

    let allSeatings = L.permutations $ replicate numBad False ++ replicate numGood True
    -- can discard length of groupings becaues each grouping will be the same
    -- length (by symmetry)
    let seatings = map head . L.group . L.sort $ allSeatings

    putStrLn $ "All possible games with " ++ (show numPlayers) ++ " players, modded out by symmetry:"
    putStrLn ""

    putStr "Arbitrary meta:       "
    print $ foldl1 (<>) . map (goodTeamRatioArbitaryMeta numPlayers) $ seatings

    putStr "Two-to-the-left meta: "
    print $ foldl1 (<>) . map (goodTeamRatioTwoToLeftMeta numPlayers) $ seatings
    putStrLn "-------------------------------------------------------------------"

main :: IO ()
main = mapM_ evaluateMetasForNumPlayers [5..10]
