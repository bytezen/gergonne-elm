module Board exposing (..)

import Sandbox.DevData exposing (deck)

import Card
import Svg exposing (Svg,g,rect)
import Svg.Attributes exposing (id,transform,height,width,style,x,y,fill)
import List.Extra exposing (groupsOf)
import State exposing (State)


--view : List (Card.Card) -> Svg msg
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
             
    {-
    g
     []
     [ 
     rect 
        [width "50", height "50", x "10" , y "10", fill "red"] 
        []
     ]
     -}


-- State is the offset to apply to the row
-- Value is the row to transform
offset = 10
boardWidth = 500
rowVOffset = 50
rowHOffset = 200

layout =
   State.run 0 <|  
    State.map 
        ((+) offset)
        <| (State.advance 
                <| (\int -> (100,100)
                    )
           )  
 
layoutAll =
    State.traverse
        (\off -> State.advance (\_ -> ("foo" ++ (toString off), off + offset)))
        (List.range 1 10)

bump = State.modify ((+) offset)