module UI.Component.Board exposing (..)

import Sandbox.DevData exposing (deck)

import Svg exposing (Svg,Attribute,g,rect,svg)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)
import Animation
import Color 



--view : List (Card.Card) -> Svg msg
type alias Model = 
    {
      cards : List Card.Card
    , styles : List Animation.State --List Animation.Property
    , dimension : Dimensions 
    }

init : Model
init = 
    {
    styles = 
        List.map Animation.style 
            [
                [
                  Animation.fill Color.red
                --, Animation.translate pos2x pos2y
                , Animation.x 192.99
                , Animation.y 107.392
                , Animation.width <| Animation.px 107.676
                , Animation.height <| Animation.px 108.167
                ]
            ]
    , 
    dimension = Dimensions 325 325
    , cards = [dummyCard]
    }

foo : Model -> Bool
foo {dimension} = 
    case dimension of
        (Dimensions w h) ->
            False

dummyCard : Card.Card
dummyCard = Card.Card (Card.rank "diamonds") 10

defaultSize : Dimensions
defaultSize = Dimensions 600 400
--view : Model -> msg  -> Svg msg
--view model msg =
view : Model -> msg -> Svg msg
view {styles, cards} msg =
    svg
        [ version "1.1"
        , x "0"
        , y "0"
        , viewBox "0 0 323.141 322.95"
        ]
      <|
        showDealt styles cards
        --[ rect 
        --    (Animation.render model.styles)
        --    []
        --]

        --[ rect
        --    ([ fill "#7FD13B"
        --    , x "192.99"
        --    , y "107.392"
        --    , width "107.676"
        --    , height "108.167"
        --    --, transform "matrix(0.7071 0.7071 -0.7071 0.7071 186.4727 -127.2386)"
        --    --, transform "matrix(0.7071 0.7071 -0.7071 0.7071 86.4727 -127.2386)"
        --    , onClick msg
        --    ] -- ++ (Animation.render model.styles)
        --    )
        --    []
        --]

showDealt : List Animation.State -> List Card.Card -> List (Svg msg)
showDealt styles cards =
      List.map 
      --(\c -> Card.view {card = c, styleProps = Card.initStyle}) 
      (\c -> 
        Card.view { card = c
                  --, styleProps = Card.initStyle
                  , styleProps = styles
                  }
      ) 
      cards


position1 = "matrix(0.7071 0.7071 -0.7071 0.7071 86.4727 -127.2386)"
angle1 = Animation.deg 45
angle2 = Animation.deg 90
pos1x = Animation.px 0 --86
pos1y = Animation.px 0 --127
pos2x = Animation.px 186
pos2y = Animation.px -127
position2 = "matrix(0.7071 0.7071 -0.7071 0.7071 186.4727 -127.2386)"

style1 : Animation.Property
style1 = Animation.translate pos1x pos1y