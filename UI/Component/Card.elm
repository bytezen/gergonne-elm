module UI.Component.Card exposing (..)

import Svg exposing (Svg, g, text, rect)
import Svg.Attributes exposing (x,y,width,height,stroke,fill,rx,ry,style)
import Color exposing (rgb)
import Animation


type Rank = Rank
type alias Val = Int

type Card = Card Rank Val

type alias Palette = 
        {
          gray : Color.Color
        , green : Color.Color
        }

type alias Model = 
  {
    card : Card
  , styleProps : List (Animation.State)
  }


initStyle =
  cardStyle 

card : Rank -> Val -> Card
card r v = Card r v        

rank : a -> Rank
rank _ = Rank

value : Model -> Val
value {card} = 
  case card of
    (Card _ v ) -> v

palette : Palette 
palette =
    {
      gray = Color.lightCharcoal
    , green = Color.darkGreen
    }


view : Model -> Svg msg
view {card, styleProps} =
    let
        v = case card of
              (Card _ val ) -> val

        --bg = rect cardAttributes []
        --bg = rect (Animation.style styleProps |> Animation.render) []
        bg = List.map 
                (\s -> 
                  rect (Animation.render s) []
                )
                styleProps
              
        val = Svg.text_ 
                [
                  x <| toString valX
                , y <| toString valY
                , fill <| showColor valFill 
                ] 
                [ Svg.text <| toString v ]
    in
        g 
          [style "font-size: 10pt;"] 
          (bg ++ [val])
            

render : Svg msg
render = 
    rect 
        cardAttributes
        []


-- layout
valX = 0.80 * cardWidth
valY = 0.15 * cardHeight
valFill = Color.red


-- Card dimensions

cardFill = "orange"
cardRx = 10.0
cardRy = 10.0
cardWidth = 80.0
cardHeight = cardWidth / cardAR 
cardAR = 2.5 / 3.5 
cardX = 0.0 -- -cardWidth * 0.5 
cardY = 0.0 -- -cardHeight * 0.5 
--cardScale = 1.0


cardStyle : List Animation.Property
cardStyle =
  [
    Animation.attr "rx" cardRx "px"
  , Animation.attr "ry" cardRy "px" 
  , Animation.width <| Animation.px cardWidth
  , Animation.height <| Animation.px cardHeight
  , Animation.fill Color.blue
  ]

cardAttributes =
    let
        attrFn = 
            [ 
              ( rx, cardRx )
            , ( ry, cardRy )
            , ( width, cardWidth )
            , ( height, cardHeight )
            , ( x, cardX )
            , ( y, cardY )
            ]

        colors = [
                   ( fill, palette.gray )
                 , ( stroke, palette.green )  
                 ]

        floatAttr = List.map 
                        ( \ (f,val) ->
                            f (toString val)
                        )
                        attrFn

        colorAttr = List.map
                        (\ (f,val) -> 
                            f (showColor val)
                        )
                        colors
    in
        floatAttr ++ colorAttr


-- UTILS

--showFloat : Float -> String
--showFloat = toString

showColor : Color.Color -> String
showColor c =
    let
       {red,green,blue,alpha} = Color.toRgb c
       showR = toString red
       showG = toString green
       showB = toString blue
       rgbstr = String.join "," [showR,showG,showB]
    in
        "rgb(" ++ rgbstr ++ ")" 