module Sanddbox.StageCard exposing (..)

import Html exposing (Html)
import Svg exposing (svg)

import UI.Component.Card as Card

type alias Model = Int

update msg model = (model, Cmd.none)

view model =
    Html.div [] 
    [Html.text (toString model)
    , svg [] 
            [Card.view testcard]
    ]


main : Program Never Model msg
main =
    Html.program
        { init = (10, Cmd.none)
        , view = view
        , update = update
        , subscriptions =
            (\model -> Sub.none )
        }

testcard : Card.Card 
testcard = Card.Card (Card.rank "diamonds") 10