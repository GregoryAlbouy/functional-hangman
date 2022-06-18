module EngineTests exposing (..)

import Engine exposing (Model, getRemainingTries, init, isLetterPicked, isLost, isOver, isStarted, isWon, pickLetter)
import Expect
import Test exposing (..)


runningModel : Model
runningModel =
    pickLetter 'l' (init { wordToGuess = "hello", maxTries = 10 })


testEmptyState : Test
testEmptyState =
    describe "empty state"
        [ test "isStarted" (\_ -> Expect.false "" (isStarted emptyModel))
        , test "isWon" (\_ -> Expect.false "" (isWon emptyModel))
        , test "isLost" (\_ -> Expect.false "" (isLost emptyModel))
        , test "isOver" (\_ -> Expect.false "" (isOver emptyModel))
        ]


testRunningState : Test
testRunningState =
    describe "running state"
        [ test "isStarted" (\_ -> Expect.true "" (isStarted initModel))
        , test "isWon" (\_ -> Expect.false "" (isWon initModel))
        , test "isLost" (\_ -> Expect.false "" (isLost initModel))
        , test "isOver" (\_ -> Expect.false "" (isOver initModel))
        ]


testPickLetter : Test
testPickLetter =
    let
        goodPick =
            pickLetter 'l' initModel

        badPick =
            pickLetter 'x' initModel

        duplicatePick =
            pickLetter 'x' badPick
    in
    describe "pickLetter"
        [ test "good pick adds picked letter" (\_ -> Expect.true "" (isLetterPicked 'l' goodPick))
        , test "bad pick adds picked letter" (\_ -> Expect.true "" (isLetterPicked 'x' badPick))
        , test "good pick does not decrement count" (\_ -> Expect.equal 10 (getRemainingTries goodPick))
        , test "bad pick decrements count" (\_ -> Expect.equal 9 (getRemainingTries badPick))
        , test "duplicate pick is noop" (\_ -> Expect.equal badPick duplicatePick)
        ]


emptyModel : Model
emptyModel =
    Engine.empty


initModel : Model
initModel =
    init { wordToGuess = "hello", maxTries = 10 }


modelWithPick : Char -> Model
modelWithPick letter =
    pickLetter letter initModel
