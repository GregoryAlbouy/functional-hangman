module EngineTests exposing (..)

import Engine exposing (Model, getRemainingTries, getWordRepr, init, isLetterPicked, isLost, isOver, isStarted, isWon, pickLetter)
import Expect
import Test exposing (..)


testIsStarted : Test
testIsStarted =
    let
        cases =
            [ { name = "empty is not started"
              , model = Engine.empty
              , exp = Expect.false
              }
            , { name = "init is started"
              , model = initModel
              , exp = Expect.true
              }
            , { name = "won is started"
              , model = wonModel
              , exp = Expect.true
              }
            , { name = "lost is started"
              , model = lostModel
              , exp = Expect.true
              }
            ]

        run c =
            test c.name (\_ -> c.exp "" (isStarted c.model))
    in
    describe "isStarted" (List.map run cases)


testIsWon : Test
testIsWon =
    let
        cases =
            [ { name = "empty is not won"
              , model = Engine.empty
              , exp = Expect.false
              }
            , { name = "init is not won"
              , model = initModel
              , exp = Expect.false
              }
            , { name = "won is won"
              , model = wonModel
              , exp = Expect.true
              }
            , { name = "lost is not won"
              , model = lostModel
              , exp = Expect.false
              }
            ]

        run c =
            test c.name (\_ -> c.exp "" (isWon c.model))
    in
    describe "isWon" (List.map run cases)


testIsLost : Test
testIsLost =
    let
        cases =
            [ { name = "empty is not lost"
              , model = Engine.empty
              , exp = Expect.false
              }
            , { name = "init is not lost"
              , model = initModel
              , exp = Expect.false
              }
            , { name = "won is not lost"
              , model = wonModel
              , exp = Expect.false
              }
            , { name = "lost is lost"
              , model = lostModel
              , exp = Expect.true
              }
            ]

        run c =
            test c.name (\_ -> c.exp "" (isLost c.model))
    in
    describe "isLost" (List.map run cases)


testIsOver : Test
testIsOver =
    let
        cases =
            [ { name = "empty is not over"
              , model = Engine.empty
              , exp = Expect.false
              }
            , { name = "init is not over"
              , model = initModel
              , exp = Expect.false
              }
            , { name = "won is over"
              , model = wonModel
              , exp = Expect.true
              }
            , { name = "lost is over"
              , model = lostModel
              , exp = Expect.true
              }
            ]

        run c =
            test c.name (\_ -> c.exp "" (isOver c.model))
    in
    describe "isOver" (List.map run cases)


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
        , test "good pick does not decrement count" (\_ -> Expect.equal 3 (getRemainingTries goodPick))
        , test "bad pick decrements count" (\_ -> Expect.equal 2 (getRemainingTries badPick))
        , test "duplicate pick is noop" (\_ -> Expect.equal badPick duplicatePick)
        , test "game over pick is noop" (\_ -> Expect.equal lostModel gameOverPick)
        ]


testGetWordRepr : Test
testGetWordRepr =
    let
        cases =
            [ { name = "hides letters not found"
              , model = initModel
              , exp = [ '_', '_', '_', '_', '_' ]
              }
            , { name = "reveals found letters"
              , model = initModel |> pickLetter 'h' |> pickLetter 'l'
              , exp = [ 'h', '_', 'l', 'l', '_' ]
              }
            , { name = "reveals word on win"
              , model = wonModel
              , exp = [ 'h', 'e', 'l', 'l', 'o' ]
              }
            , { name = "reveals word on lost"
              , model = lostModel
              , exp = [ 'h', 'e', 'l', 'l', 'o' ]
              }
            , { name = "returns empty set for empty model"
              , model = Engine.empty
              , exp = []
              }
            ]

        run c =
            test c.name (\_ -> Expect.equalLists c.exp (getWordRepr c.model '_'))
    in
    describe "getWordRepr" (List.map run cases)


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
