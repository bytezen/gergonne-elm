module Sandbox.AnimateCard exposing (..)

import Time exposing (second)
import Html exposing (h1, div, Html)
import Html.Attributes as Attr
import Html.Events exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Animation exposing (px)
import Color exposing (purple, green, rgb)
import List.Extra as L 


type alias Model =
    { styles : List Animation.State
    , index : Int
    , target : Animation.State
    --, cardStyle : List Animation.State
    }


type Msg
    = EverybodySwitch
    | Animate Animation.Msg


type alias Palette =
    { orange : Color.Color
    , green : Color.Color
    , lavender : Color.Color
    , blue : Color.Color
    }


palette : Palette
palette =
    { orange = rgb 240 173 0
    , green = rgb 127 209 59
    , lavender = rgb 90 99 120
    , blue = rgb 96 181 204
    }


offscreen : Animation.Property
offscreen = 
    let
        cardWidth = 80
        cardHeight = 120
    in
        Animation.points 
            [ ( 0.0, 0.0 )
            , ( 0.0, 80 )
            , ( 0.0, cardWidth )
            , ( cardWidth, cardHeight )
            , ( 80, 120 )
            , ( 0.0, 120)
            , ( 0.0, cardHeight)
            ]


-- Utils

gridPos : Int -> (Int,Int)
gridPos i =
    let
        xoff = (flip rem) 3
        yoff = (flip (//)) 3
    in
    (yoff i,xoff i)


rowsOf3 = L.groupsOf 3
columns = L.transpose
column1 = columns >> List.head >> (Maybe.withDefault [])
column2 = columns >> List.drop 1 >> List.head >> (Maybe.withDefault [])
column3 = columns >> L.last >> (Maybe.withDefault [])


-- choosing columns

-- given a sort target, selected column, and list of unsorted numbers
-- return the resulting list after the cards are "picked up"
--gergonneSortOn : Int -> GergonneColumn -> List Int -> List Int
gergonneSortOn target col vals =
        []



            


{- Example

deck = List.range 1 27 -- make this random
deal = rowsOf3 deck
(top, mid, bot ) = (column1 deal, column2 deal, column3 deal)
newdeck = List.concat [top, mid, bot]
newdeal = rowsOf3 newdeck

-}


cards : List (List Animation.Property)
cards =
    let
        rowOffset = 50
        colOffset = 205
        positions = List.map gridPos <| List.range 0 26
        toAnimationProperty (row,col) = 
            [ Animation.x <| toFloat (colOffset * col)
            , Animation.y <| toFloat (rowOffset * row)
            , Animation.width (px 200)
            , Animation.height (px 280)
            , Animation.fill palette.orange
            , Animation.stroke Color.black
            , Animation.custom "rx" 10 "px"
            , Animation.custom "ry" 10 "px"
            ]
    in
        List.map toAnimationProperty positions
            
    --[  [ Animation.x 0 
    --    ,Animation.y 0
    --   ]
    --]
--cardProperty =
--    [  [ Animation.top (px 0) ]
--      ,[ Animation.left (px 0)]
--    ]


polygons : List (List Animation.Property)
polygons =
    [ [ Animation.points
            [ ( 161.649, 152.782 )
            , ( 231.514, 82.916 )
            , ( 91.783, 82.916 )
            ]
      , Animation.fill palette.orange
      ]
    , [ Animation.points
            [ ( 8.867, 0 )
            , ( 79.241, 70.375 )
            , ( 232.213, 70.375 )
            , ( 161.838, 0 )
            ]
      , Animation.fill palette.green
      ]
    , [ Animation.points
            [ ( 323.298, 143.724 )
            , ( 323.298, 0 )
            , ( 179.573, 0 )
            ]
      , Animation.fill palette.blue
      ]
    , [ Animation.points
            [ ( 152.781, 161.649 )
            , ( 0, 8.868 )
            , ( 0, 314.432 )
            ]
      , Animation.fill palette.lavender
      ]
    , [ Animation.points
            [ ( 255.522, 246.655 )
            , ( 323.298, 314.432 )
            , ( 323.298, 178.879 )
            ]
      , Animation.fill palette.orange
      ]
    , [ Animation.points
            [ ( 161.649, 170.517 )
            , ( 8.869, 323.298 )
            , ( 314.43, 323.298 )
            ]
      , Animation.fill palette.blue
      ]
    ]


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        EverybodySwitch ->
            let
                wrappedIndex =
                    if List.length model.styles < model.index then
                        model.index - List.length model.styles
                    else
                        model.index

                newStyles =
                    (List.drop wrappedIndex polygons) ++ (List.take wrappedIndex polygons)
            in
                ( { model
                    | index = wrappedIndex + 1
                    , styles =
                        List.map2 --List.map3
                            (\i style -> --newStyle ->
                                Animation.interrupt
                                    [ Animation.wait (toFloat i * 0.05 * second)
                                    , Animation.to [Animation.fill Color.blue] -- [offscreen] -- newStyle  
                                    ]
                                    style
                            )
                            (List.range 0 (List.length model.styles))
                            model.styles
                            --newStyles
                  }
                , Cmd.none
                )

        Animate time ->
            ( { model
                | styles = List.map (Animation.update time) model.styles
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div
        [ onClick EverybodySwitch
        , Attr.style [ ( "margin", "200px auto" ), ( "width", "500px" ), ( "height", "500px" ), ( "cursor", "pointer" ) ]
        ]
        [ h1 [] [ text "Click to morph!" ]
        , svg 
            [ version "1.1"
            , x "0"
            , y "0"
            , viewBox "0 0 650 650"
            ]
          <|
            [
              Svg.g []
                [ rect
                    [ fill "#7FD13B"
                    , x "192.99"
                    , y "107.392"
                    , width "107.676"
                    , height "108.167"
                    , transform "matrix(0.7071 0.7071 -0.7071 0.7071 186.4727 -127.2386)"
                    ]
                    []
                , Svg.g []
                        (List.map ( \cardstyle -> rect (Animation.render cardstyle) []) model.styles)
                ]
            ]
        --, svg
        --    [ version "1.1"
        --    , x "0"
        --    , y "0"
        --    , viewBox "0 0 323.141 322.95"
        --    ]
        --  <|
        --    [ rect
        --        [ fill "#7FD13B"
        --        , x "192.99"
        --        , y "107.392"
        --        , width "107.676"
        --        , height "108.167"
        --        , transform "matrix(0.7071 0.7071 -0.7071 0.7071 186.4727 -127.2386)"
        --        ]
        --        []
        --    , Svg.g []
        --        (List.map (\poly -> polygon (Animation.render poly) []) model.styles)
        --    ]
        ]


init : ( Model, Cmd Msg )
init =
    ( { styles = List.map Animation.style cards --List.map Animation.style polygons
      , index = 1
      , target = Animation.style [offscreen]
      --, cardStyle = List.map Animation.style cardProperty
      }
    , Cmd.none
    )


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions =
            (\model ->
                Animation.subscription
                    Animate
                    model.styles
            )
        }