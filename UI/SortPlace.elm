module UI.SortPlace exposing (..)

import Html exposing (Html, div, text, h1)
import Html.Attributes
import Html.Events exposing (onClick)

import UI.Component.Board as Board


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
    PlaceValue 


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
view model =
    let
        currentPlace = Units --model.currentPlace
        placeValue =
            case model of
                Units -> "Units"
                Threes -> "Threes"
                Nines -> "Nines"
    in
            
    div []
        [ h1 
            --[onClick (ForParent (column1selected currentPlace))] 
            [onClick <| ForMe Hovering ] 
            [text ("Select column ..." ++ placeValue )
            ]
        , Board.view (ForMe Hovering)
        ]


update : InternalMsg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Hovering ->
        (Nines , Cmd.none)
    --(model, Cmd.none)


column1selected : PlaceValue -> OutMsg
column1selected = 
    Select 1


init : Model
init = Units 
