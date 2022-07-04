module GameEngineTests exposing (testChancesLeft, testPickLetter, testState, testWordRepr)

import Expect
import Game.Engine exposing (End(..), Model, State(..), chancesLeft, init, isLetterPicked, pickLetter, state, wordRepr)
import Test exposing (Test, describe, test)



-- STUBS


initModel : Model
initModel =
    init "hello" 3


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



-- TESTS


testState : Test
testState =
    [ { name = "empty -> NotStarted"
      , model = Game.Engine.empty
      , exp = Expect.equal NotStarted
      }
    , { name = "init -> Running"
      , model = initModel
      , exp = Expect.equal Running
      }
    , { name = "won -> Ended Victory"
      , model = wonModel
      , exp = Expect.equal (Ended Victory)
      }
    , { name = "lost -> Ended Defeat"
      , model = lostModel
      , exp = Expect.equal (Ended Defeat)
      }
    ]
        |> runTestCases "testState" state


testPickLetter : Test
testPickLetter =
    let
        run : { a | name : String, exp : Bool -> Expect.Expectation, model : Model, pickedLetter : Char } -> Test
        run c =
            test c.name <|
                \_ ->
                    c.exp
                        (c.model
                            |> pickLetter c.pickedLetter
                            |> isLetterPicked c.pickedLetter
                        )
    in
    describe "pickLetter" <|
        ([ { name = "good pick adds picked letter"
           , model = initModel
           , pickedLetter = 'l'
           , exp = Expect.true ""
           }
         , { name = "bad pick adds picked letter"
           , model = initModel
           , pickedLetter = 'x'
           , exp = Expect.true ""
           }
         , { name = "post-win pick does not add picked letter"
           , model = wonModel
           , pickedLetter = 'x'
           , exp = Expect.false ""
           }
         , { name = "post-lose does not add picked letter"
           , model = lostModel
           , pickedLetter = 'l'
           , exp = Expect.false ""
           }
         ]
            |> List.map run
        )


testChancesLeft : Test
testChancesLeft =
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
        |> runTestCases "chancesLeft" chancesLeft


testWordRepr : Test
testWordRepr =
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
      , model = Game.Engine.empty
      , exp = Expect.equalLists []
      }
    ]
        |> runTestCases "wordRepr" (wordRepr '_')



-- HELPERS


type alias StateCase a =
    { name : String
    , model : Model
    , exp : a -> Expect.Expectation
    }


runTestCases : String -> (Model -> a) -> List (StateCase a) -> Test
runTestCases desc fn cases =
    let
        run : StateCase a -> Test
        run c =
            test c.name (\_ -> c.exp (fn c.model))
    in
    describe desc (List.map run cases)
