module Gergonne exposing (..)

import Html exposing (h1, div, Html)
import Html.Attributes as Attr
import Svg exposing (svg,rect)
import Svg.Attributes exposing (version,x,y,viewBox, width,height, fill)
import Color
import Animation exposing (px)

import UI.Component.Card as Card



type Msg = 
    Msg



type alias Model = 
   { 
     cards : List Card.Card
   , styles : List (List Animation.State)
   }


init : ( Model, Cmd Msg )
init =
    {
     cards = []
    ,styles = [] 
    }
    ! [Cmd.none]


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    model ! [Cmd.none]
    --case msg of
    --    UIMsg uimsg ->
    --        let
    --            (newuimodel, cmd ) =
    --                UI.update uimsg model.ui
    --        in
    --            {model | ui = newuimodel}
    --            !
    --            [Cmd.map UIMsg cmd]


    --    _ -> 
    --        (model, Cmd.none)


view : Model -> Html Msg
view model =
    svg
        [ version "1.1"
        , x "0"
        , y "0"
        , viewBox "0 0 323.141 322.95"
        ]    
        [rect 
            [width "200", height "200", fill "red"]
            []
        ]
    --div [][]
    --Html.map UIMsg (UI.view model.ui)


subscriptions : Model -> Sub Msg
subscriptions model = 
    Sub.none
    --Animation.subscription 
    --    (UIMsg << UI.Animate)
    --    [Animation.style model.ui.styles]
    --UI.subscription model.ui--Sub.none

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
            --(\model ->
            --    Animation.subscription
            --        Animate
            --        model.styles
            --)
        }


-- CARD Styles
deck : List Card.Card
deck = 
    List.map
        (\i -> 
            case (rem i 4) of
                0 ->
                    Card.Card (Card.rank "clubs") i 
                1 -> 
                    Card.Card (Card.rank "hearts") i 
                2 -> 
                    Card.Card (Card.rank "spades") i 
                _ -> 
                    Card.Card (Card.rank "diamonds") i 
        )
        (List.range 1 27)
