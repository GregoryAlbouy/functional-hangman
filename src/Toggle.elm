module Toggle exposing (State(..), toggle)


type State
    = On
    | Off


toggle : State -> State
toggle state =
    case state of
        On ->
            Off

        Off ->
            On
