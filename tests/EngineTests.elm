module EngineTests exposing (..)

import Engine exposing (Model, getRemainingTries, getWordRepr, init, isLetterPicked, isLost, isOver, isStarted, isWon, pickLetter)
import Expect
import Test exposing (..)


testIsStarted : Test
testIsStarted =
    [ { name = "empty is not started"
      , model = Engine.empty
      , exp = Expect.false ""
      }
    , { name = "init is started"
      , model = initModel
      , exp = Expect.true ""
      }
    , { name = "won is started"
      , model = wonModel
      , exp = Expect.true ""
      }
    , { name = "lost is started"
      , model = lostModel
      , exp = Expect.true ""
      }
    ]
        |> runTestCases "isStarted" isStarted


testIsWon : Test
testIsWon =
    [ { name = "empty is not won"
      , model = Engine.empty
      , exp = Expect.false ""
      }
    , { name = "init is not won"
      , model = initModel
      , exp = Expect.false ""
      }
    , { name = "won is won"
      , model = wonModel
      , exp = Expect.true ""
      }
    , { name = "lost is not won"
      , model = lostModel
      , exp = Expect.false ""
      }
    ]
        |> runTestCases "isWon" isWon


testIsLost : Test
testIsLost =
    [ { name = "empty is not lost"
      , model = Engine.empty
      , exp = Expect.false ""
      }
    , { name = "init is not lost"
      , model = initModel
      , exp = Expect.false ""
      }
    , { name = "won is not lost"
      , model = wonModel
      , exp = Expect.false ""
      }
    , { name = "lost is lost"
      , model = lostModel
      , exp = Expect.true ""
      }
    ]
        |> runTestCases "isLost" isLost


testIsOver : Test
testIsOver =
    [ { name = "empty is not over"
      , model = Engine.empty
      , exp = Expect.false ""
      }
    , { name = "init is not over"
      , model = initModel
      , exp = Expect.false ""
      }
    , { name = "won is over"
      , model = wonModel
      , exp = Expect.true ""
      }
    , { name = "lost is over"
      , model = lostModel
      , exp = Expect.true ""
      }
    ]
        |> runTestCases "isOver" isOver


testPickLetter : Test
testPickLetter =
    let
        goodPick =
            pickLetter 'l' initModel

        badPick =
            pickLetter 'x' initModel

        duplicatePick =
            pickLetter 'x' badPick

        gameOverPick =
            pickLetter 'z' lostModel
    in
    describe "pickLetter"
        [ test "good pick adds picked letter" (\_ -> Expect.true "" (isLetterPicked 'l' goodPick))
        , test "bad pick adds picked letter" (\_ -> Expect.true "" (isLetterPicked 'x' badPick))
        , test "duplicate pick is noop" (\_ -> Expect.equal badPick duplicatePick)
        , test "game over pick is noop" (\_ -> Expect.equal lostModel gameOverPick)
        ]


testGetRemainingTries : Test
testGetRemainingTries =
    [ { name = "good pick does not decrement"
      , model = initModel |> pickLetter 'l'
      , exp = Expect.equal 3
      }
    , { name = "bad pick decrements"
      , model = initModel |> pickLetter 'x'
      , exp = Expect.equal 2
      }
    , { name = "duplicate pick does not decrement"
      , model = initModel |> pickLetter 'x' |> pickLetter 'x'
      , exp = Expect.equal 2
      }
    , { name = "post-win pick does not decrement"
      , model = wonModel |> pickLetter 'x'
      , exp = Expect.equal 3
      }
    , { name = "post-lose pick does not decrement"
      , model = lostModel |> pickLetter 'x'
      , exp = Expect.equal 0
      }
    ]
        |> runTestCases "getRemainingTries" getRemainingTries


testGetWordRepr : Test
testGetWordRepr =
    [ { name = "hides letters not found"
      , model = initModel
      , exp = Expect.equalLists [ '_', '_', '_', '_', '_' ]
      }
    , { name = "reveals found letters"
      , model = initModel |> pickLetter 'h' |> pickLetter 'l'
      , exp = Expect.equalLists [ 'h', '_', 'l', 'l', '_' ]
      }
    , { name = "reveals word on win"
      , model = wonModel
      , exp = Expect.equalLists [ 'h', 'e', 'l', 'l', 'o' ]
      }
    , { name = "reveals word on lost"
      , model = lostModel
      , exp = Expect.equalLists [ 'h', 'e', 'l', 'l', 'o' ]
      }
    , { name = "returns empty set for empty model"
      , model = Engine.empty
      , exp = Expect.equalLists []
      }
    ]
        |> runTestCases "getWordRepr" (getWordRepr '_')


initModel : Model
initModel =
    init { wordToGuess = "hello", maxTries = 3 }


lostModel : Model
lostModel =
    initModel
        |> pickLetter 'a'
        |> pickLetter 'b'
        |> pickLetter 'c'


wonModel : Model
wonModel =
    initModel
        |> pickLetter 'h'
        |> pickLetter 'e'
        |> pickLetter 'l'
        |> pickLetter 'o'


type alias StateCase a =
    { name : String
    , model : Model
    , exp : a -> Expect.Expectation
    }


runTestCases : String -> (Model -> a) -> List (StateCase a) -> Test
runTestCases desc fn cases =
    let
        run c =
            test c.name (\_ -> c.exp (fn c.model))
    in
    describe desc (List.map run cases)
