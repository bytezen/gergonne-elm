module Magic exposing (..)

import Html exposing (h1, div, Html, text)
import Html.Events exposing (..)

import Sandbox.DevData as Data

import Dict exposing (Dict)
import Svg exposing (Svg,svg,rect)
import Svg.Attributes exposing (width,height,fill,x,y)
import Card
import Board
--import Screen

-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of 
        ChangeScreen screen ->
            ({ model | screen = screen }
            , Cmd.none
            )


-- MODEL
type alias Model =
    { screen : Screen
    }


init : (Model, Cmd Msg) 
init = ({
        screen = welcomeScreen
       }
       , Cmd.none
       )


-- VIEW
view : Model -> Html Msg
view model =
    viewScreen model.screen


viewScreen : Screen -> Html Msg
viewScreen screen =
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

viewSvgTest : Svg msg
viewSvgTest = 
    svg 
        [ width "250", height "350", fill "red"]
        [ 
         Card.view <| Card.card (Card.rank "foo") 10 
        ]
-- MSG
type Msg = ChangeScreen Screen


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

type alias UIModel = Dict String Screen

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

uimodel : UIModel
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




-- MAIN

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
            --(\model ->
            --    Animation.subscription
            --        Animate
            --        model.styles
            --)
        }