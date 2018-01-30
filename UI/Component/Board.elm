module UI.Component.Board exposing (..)

import Svg exposing (Svg,Attribute,g,rect,svg)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)



type Dimensions = Dimensions Int Int

type alias Model = 
    {
      --cards : List Int
    --, 
    dimension : Dimensions 
    }

foo : Model -> Bool
foo {dimension} = 
    case dimension of
        (Dimensions w h) ->
            False

--view : Model -> msg  -> Svg msg
--view model msg =
view msg =
    svg
        [ version "1.1"
        , x "0"
        , y "0"
        , viewBox "0 0 323.141 322.95"
        ]
      <|
        [ rect
            [ fill "#7FD13B"
            , x "192.99"
            , y "107.392"
            , width "107.676"
            , height "108.167"
            , transform "matrix(0.7071 0.7071 -0.7071 0.7071 186.4727 -127.2386)"
            , onClick msg
            ]
            []
        ]

{-
view cards = 
    let
        -- 3 is the base number; add this to the model for the
        -- general trick
        rows = groupsOf 3 cards
        cWidth = boardWidth / 3.0 
            
    in
        g
        [id "board"]
        <| 
        List.indexedMap renderRow rows 

--renderRow : Int -> List (List Card.Card) -> Svg msg
renderRow i cards =
   let
        translate x y = "translate (" 
                        ++ (toString x) 
                        ++ ","
                        ++ (toString y) 
                        ++ ")"

        viewRow j c =
            g 
                [transform (translate (j * rowHOffset) 0 )]
                [Card.view c]
   in
        g
        [transform (translate 0 (i * rowVOffset) )]
        <| List.indexedMap 
                viewRow 
                cards 

-}