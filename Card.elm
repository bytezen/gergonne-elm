module Card exposing (..)

import Svg exposing (Svg, g, text, rect)
import Svg.Attributes exposing (x,y,width,height,stroke,fill,rx,ry,style)
import Color exposing (rgb)


type Rank = Rank
type alias Val = Int

type Card = Card Rank Val

type alias Palette = 
        {
          gray : Color.Color
        , green : Color.Color
        }


card : Rank -> Val -> Card
card r v = Card r v        

rank : a -> Rank
rank _ = Rank

value : Card -> Val
value (Card _ v ) = v

palette : Palette 
palette =
    {
      gray = Color.lightCharcoal
    , green = Color.darkGreen
    }


view : Card -> Svg msg
view (Card r v) =
    let
        bg = rect cardAttributes []
        val = Svg.text_ 
                [
                  x <| toString valX
                , y <| toString valY
                , fill <| showColor valFill 
                ] 
                [ Svg.text <| toString v ]
    in
        g 
          [style "font-size: 24pt;"] 
          [bg,val]
            

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
cardWidth = 200.0
cardHeight = cardWidth / cardAR 
cardAR = 2.5 / 3.5 
cardX = 0.0 -- -cardWidth * 0.5 
cardY = 0.0 -- -cardHeight * 0.5 
--cardScale = 1.0

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