module UI exposing (Msg, view, Screen, welcomeScreen)

import Html exposing (Html, text, div, h1)
import Html.Events exposing (onClick)
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (width, height)

import Dict exposing (Dict)
import PlaceValue 
import Board
import Sandbox.DevData as Data


-- MSG
type Msg = ChangeScreen Screen
         | Select PlaceValue.PlaceValue


-- TYPE SCREEN

type alias ScreenConfig =
    { id: String
    , title: String 
    , body: String
    }

type Screen = Welcome ScreenConfig 
            | Instructions ScreenConfig
            | Pick ScreenConfig
            | SelectColumn ScreenConfig

type alias Model = Dict String Screen


view : Screen -> Html Msg
view screen =
    let
        config = screenConfig screen 

        next = Dict.get config.id uimodel
                |> Maybe.withDefault screen
    in
        div []
            [
              h1 
                [] 
                [ text config.title ]
            , div [onClick (ChangeScreen next)]
                [ text config.body ]
            --, viewSvgTest
            , svg [ width "600", height "600"] [viewBoard]
            ]

viewBoard : Svg msg
viewBoard = Board.view Data.deck


welcomeScreen = Welcome { title = "Prepare to have your circuits blown..."
                        , body = "lorum ipsum blaga da bloop stoop droop noope"
                        , id = "welcome"
                        }

instructionScreen = Instructions 
                        { title = "How this works"
                        , body = "lorum ipsum blaga da bloop stoop droop noope"
                        , id = "instruction"
                        }
                         
pickScreen = Pick 
                { title = "Prepare to have your circuits blown..."
                , body = "lorum ipsum blaga da bloop stoop droop noope"
                , id = "pick"
                }
                         
selectColumn = SelectColumn 
                    { title = "Prepare to have your circuits blown..."
                         , body = "lorum ipsum blaga da bloop stoop droop noope"
                         , id = "selectColumn"
                     }

uimodel : Model
uimodel = Dict.fromList
                [ ( "welcome" , instructionScreen )
                ]


screenConfig : Screen -> ScreenConfig
screenConfig screen = 
                case screen of 
                    Welcome config -> config
                    Instructions config -> config
                    Pick config -> config
                    SelectColumn config -> config

nextScreen : Screen -> Screen
nextScreen screen =
    let
        id = .id <| screenConfig screen
    in
        Dict.get id uimodel
            |> Maybe.withDefault screen
