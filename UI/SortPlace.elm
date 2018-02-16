module UI.SortPlace exposing (..)

import Html exposing (Html, div, text, h1)
import Html.Attributes
import Html.Events exposing (onClick)
import Animation
import UI.Component.Board as Board
import UI.Component.Card as Card


type PlaceValue = 
    Units | Threes | Nines


type alias Column = 
    Int


type InternalMsg = 
    Hovering

type OutMsg = 
    Select Column PlaceValue -- column selected


type Msg = 
    ForParent OutMsg | ForMe InternalMsg


type alias Model = 
    {
      styles : List Animation.State 
    , cards : List Card.Card  
    }


type alias TranslationDictionary msg =
    { onInternalMsg : InternalMsg -> msg
    , onColumnSelect : Int -> msg
    }


type alias Translator msg =
    Msg -> msg


translator : TranslationDictionary msg -> Translator msg
translator { onInternalMsg, onColumnSelect } msg =
    case msg of
        ForMe internal ->
            onInternalMsg internal

        ForParent (Select col place) ->
            onColumnSelect col


view : Model -> Html Msg
view {cards,styles} =
    let
        --currentPlace = Units --model.currentPlace
        --placeValue =
        --    case model of
        --        Units -> "Units"
        --        Threes -> "Threes"
        --        Nines -> "Nines"
        placeValue = "dummytesting"
        (Card.Card _ v) = case cards of
                       c::cs -> c
                       [] -> Card.Card (Card.rank "bad") 0
    in
            
    div []
        [ h1 
            [onClick (ForParent (column1selected Nines))] 
            --[onClick <| ForMe Hovering ] 
            [text ("Select column ..." ++ (toString v)) --placeValue )
            ]
        --, Board.view Board.init (ForMe Hovering)
        , Board.view 
            {cards = cards
            , styles = styles
            , dimension = Board.defaultSize
            } 
            (ForMe Hovering)
        ]

    

update : InternalMsg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Hovering ->
        (model , Cmd.none)
    --(model, Cmd.none)


column1selected : PlaceValue -> OutMsg
column1selected = 
    Select 1


init : Model
init = 
    { cards = [Card.Card (Card.rank "diamonds") 8]
    , styles = List.map Animation.style [Card.cardStyle]
    }
