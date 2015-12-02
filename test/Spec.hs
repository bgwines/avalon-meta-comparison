
import Metas

import Test.Tasty
import Test.Tasty.HUnit

{-
    seating: [0,0,1,1,1]
    round 1 (team size 2):
        [{0,0},1,1,1]
        [0,{0,1},1,1]
        [0,0,{1,1},1] -- yes (:= 1)
        [0,0,1,{1,1}] -- yes (:= 2)
        [0},0,1,1,{1]

    round 2 (team size 3):
        [{0,0,1},1,1]
        [0,{0,1,1},1]
        [0,0,{1,1,1}] -- yes (:= 3)
        [0},0,1,{1,1]
        [0,0},1,1,{1]

    round 3 (team size 2):
        [{0,0},1,1,1]
        [0,{0,1},1,1]
        [0,0,{1,1},1] -- yes (:= 4)
        [0,0,1,{1,1}] -- yes (:= 5)
        [0},0,1,1,{1]

    round 4 (team size 3):
        [{0,0,1},1,1]
        [0,{0,1,1},1]
        [0,0,{1,1,1}] -- yes (:= 6)
        [0},0,1,{1,1]
        [0,0},1,1,{1]

    round 5 (team size 3):
        [{0,0,1},1,1]
        [0,{0,1,1},1]
        [0,0,{1,1,1}] -- yes (:= 7)
        [0},0,1,{1,1]
        [0,0},1,1,{1]
-}
testGoodTeamRatioTwoToLeftMeta5Players :: Assertion
testGoodTeamRatioTwoToLeftMeta5Players = do
    let gtr = goodTeamRatioTwoToLeftMeta [False, False, True, True, True]
    gtr @?= GoodTeamRatio 7 (3 * 5) -- only concerned with Good's proposals

{-
    seating: [0,0,1,1,1,1]
    round 1 (team size 2):
        [{0,0},1,1,1,1] -- NOT CONSIDERED
        [0,{0,1},1,1,1] -- NOT CONSIDERED
        [0,0,{1,1},1,1] -- yes (:= 1) -- / 1
        [0,0,1,{1,1},1] -- yes (:= 2) -- / 2
        [0,0,1,1,{1,1}] -- yes (:= 3) -- / 3

    round 2 (team size 3):
        [0,0},1,1,1,{1] -- / 4
        [{0,0,1},1,1,1] -- NOT CONSIDERED
        [0,{0,1,1},1,1] -- NOT CONSIDERED
        [0,0,{1,1,1},1] -- yes (:= 4) -- / 5
        [0,0,1,{1,1,1}] -- yes (:= 5) -- / 6

    round 3 (team size 4):
        [0,0},1,1,{1,1] -- / 7
        [0,0,1},1,1,{1] -- / 8
        [{0,0,1,1},1,1] -- NOT CONSIDERED
        [0,{0,1,1,1},1] -- NOT CONSIDERED
        [0,0,{1,1,1,1}] -- yes (:= 6) -- / 9

    round 4 (team size 3):
        [0,0,1,{1,1,1}] -- yes (:= 7) -- / 10
        [0},0,1,1,{1,1] -- / 11
        [0,0},1,1,1,{1] -- / 12
        [{0,0,1},1,1,1] -- NOT CONSIDERED
        [0,{0,1,1},1,1] -- NOT CONSIDERED

    round 5 (team size 4):
        [0,0,{1,1,1,1}] -- yes (:= 8) -- / 13
        [0},0,1,{1,1,1] -- / 14
        [0,0},1,1,{1,1] -- / 15
        [0,0,1},1,1,{1] -- / 16
        [{0,0,1,1},1,1] -- NOT CONSIDERED
-}
testGoodTeamRatioTwoToLeftMeta6Players :: Assertion
testGoodTeamRatioTwoToLeftMeta6Players = do
    let gtr = goodTeamRatioTwoToLeftMeta [False, False, True, True, True, True]
    gtr @?= GoodTeamRatio 8 16 -- only concerned with Good's proposals

-- Team size 5 has min. team size 2; place no two `True`s together
testGoodTeamRatioTwoToLeftMetaNoGoodTeams :: Assertion
testGoodTeamRatioTwoToLeftMetaNoGoodTeams = do
    let gtr = goodTeamRatioTwoToLeftMeta [False, True, False, True, False]
    gtr @?= GoodTeamRatio 0 (2 * 5) -- only concerned with Good's proposals

{-
    seating: [0,0,1,1,1]

    mission 1:
    all king indices: [0,1,2,3,4]
    filtered king indices: [2,3,4]
        [4,2] for king 2: True
        [3,2] for king 2: True
        [2,1] for king 2: False
        [2,0] for king 2: False
        [4,3] for king 3: True
        [3,2] for king 3: True
        [3,1] for king 3: False
        [3,0] for king 3: False
        [4,3] for king 4: True
        [4,2] for king 4: True
        [4,1] for king 4: False
        [4,0] for king 4: False

    mission 2:
    all king indices: [0,1,2,3,4]
    filtered king indices: [2,3,4]
        [4,3,2] for king 2: True
        [4,2,1] for king 2: False
        [3,2,1] for king 2: False
        [4,2,0] for king 2: False
        [3,2,0] for king 2: False
        [2,1,0] for king 2: False
        [4,3,2] for king 3: True
        [4,3,1] for king 3: False
        [3,2,1] for king 3: False
        [4,3,0] for king 3: False
        [3,2,0] for king 3: False
        [3,1,0] for king 3: False
        [4,3,2] for king 4: True
        [4,3,1] for king 4: False
        [4,2,1] for king 4: False
        [4,3,0] for king 4: False
        [4,2,0] for king 4: False
        [4,1,0] for king 4: False

    mission 3:
    all king indices: [0,1,2,3,4]
    filtered king indices: [2,3,4]
        [4,2] for king 2: True
        [3,2] for king 2: True
        [2,1] for king 2: False
        [2,0] for king 2: False
        [4,3] for king 3: True
        [3,2] for king 3: True
        [3,1] for king 3: False
        [3,0] for king 3: False
        [4,3] for king 4: True
        [4,2] for king 4: True
        [4,1] for king 4: False
        [4,0] for king 4: False

    mission 4:
    all king indices: [0,1,2,3,4]
    filtered king indices: [2,3,4]
        [4,3,2] for king 2: True
        [4,2,1] for king 2: False
        [3,2,1] for king 2: False
        [4,2,0] for king 2: False
        [3,2,0] for king 2: False
        [2,1,0] for king 2: False
        [4,3,2] for king 3: True
        [4,3,1] for king 3: False
        [3,2,1] for king 3: False
        [4,3,0] for king 3: False
        [3,2,0] for king 3: False
        [3,1,0] for king 3: False
        [4,3,2] for king 4: True
        [4,3,1] for king 4: False
        [4,2,1] for king 4: False
        [4,3,0] for king 4: False
        [4,2,0] for king 4: False
        [4,1,0] for king 4: False

    mission 5:
    all king indices: [0,1,2,3,4]
    filtered king indices: [2,3,4]
        [4,3,2] for king 2: True
        [4,2,1] for king 2: False
        [3,2,1] for king 2: False
        [4,2,0] for king 2: False
        [3,2,0] for king 2: False
        [2,1,0] for king 2: False
        [4,3,2] for king 3: True
        [4,3,1] for king 3: False
        [3,2,1] for king 3: False
        [4,3,0] for king 3: False
        [3,2,0] for king 3: False
        [3,1,0] for king 3: False
        [4,3,2] for king 4: True
        [4,3,1] for king 4: False
        [4,2,1] for king 4: False
        [4,3,0] for king 4: False
        [4,2,0] for king 4: False
        [4,1,0] for king 4: False

-}
testGoodTeamRatioArbitaryMeta :: Assertion
testGoodTeamRatioArbitaryMeta = do
    let gtr = goodTeamRatioArbitaryMeta [False, False, True, True, True]
    gtr @?= GoodTeamRatio 21 ((2 * 3 * 4) + (3 * 3 * 6)) -- 2 rounds * (3 kings (only concerned with good)) * (4 / 10 options for each proposal include the player); then similar counting argument for the 3-player team rounds

tests :: TestTree
tests = testGroup "config tests"
    [ testCase
       "Testing the two-to-the-left meta good team ratio (5 players)"
       testGoodTeamRatioTwoToLeftMeta5Players
    , testCase
      "Testing the two-to-the-left meta good team ratio (6 players)"
      testGoodTeamRatioTwoToLeftMeta6Players
    , testCase
        "Testing the two-to-the-left meta good team ratio (no good teams)"
        testGoodTeamRatioTwoToLeftMetaNoGoodTeams
    , testCase
        "Testing the arbitrary meta good team ratio"
        testGoodTeamRatioArbitaryMeta
    ]

main :: IO ()
main = defaultMain tests
