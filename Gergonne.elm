module Gergonne exposing (..)

import Html exposing (h1, div, Html)
import Html.Attributes as Attr
import Html.Events
import Svg exposing (svg,rect)
import Svg.Attributes exposing (..)
import Svg.Events
import Color
import Animation exposing (px)
import List.Extra exposing (groupsOf, transpose, last)
import Time

import UI.Component.Card as Card


type alias CardColumn = Int

type HoverState = Over CardColumn

type PlaceValue = Units | Threes | Nines

type Base3Digit = Zero | One | Two

type Base3 = Base3 (Base3Digit,Base3Digit,Base3Digit)

type Msg = 
      Msg String
    | Pickup
    | Hovering HoverState
    | UnHover
    | Animate Animation.Msg
    | Continue ScreenId
    | SelectPlaceValueColumn PlaceValue CardColumn

type ScreenId =
      ChooseCard
    | ChooseNumber
    | Intro
    | SelectColumn PlaceValue


type alias Model = 
   { 
     deck : List Card.Card
   , styles : List Animation.State
   , hovering : Maybe HoverState
   , nextView : ScreenId
   , target : Maybe Int
   , targetBase3 : Maybe Base3
   , sortPlace : Maybe PlaceValue
   , messages : Maybe String
   , unitColumn: Maybe CardColumn
   , threesColumn : Maybe CardColumn
   , ninesColumn : Maybe CardColumn
   }


init : ( Model, Cmd Msg )
init =
    {
     deck = deck
    --,styles = List.map 
    --                (\props -> Animation.style props) 
    --                deckStyle    

    --,styles = List.map 
    --                (\props -> Animation.style props) 
    --                cardDealtStyle

    ,styles = createStyle columnAnimationStyle
                --List.map 
                    --(\props -> Animation.style props) 
                    --Animation.style 
                    --columnAnimationStyle


    ,hovering = Nothing
    ,nextView = Intro
    ,target = Nothing
    ,targetBase3 = Nothing
    ,sortPlace = Nothing
    ,messages = Nothing
    ,unitColumn = Nothing
    ,threesColumn = Nothing
    ,ninesColumn = Nothing
    }
    ! [Cmd.none]


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Msg s ->
            {model | messages = Just s} ! [Cmd.none]

        Pickup ->
            {model | styles =  
                    --List.map 
                    --(\props -> Animation.style props) 
                    --deckStyle
                pickupAnimationStyles model.styles
                --List.map 
                --    (\style -> --newStyle ->
                --        Animation.interrupt
                --            [ 
                --              --Animation.wait (toFloat i * 0.05 * second)
                --             Animation.to [Animation.translate (px 100) (px 100)
                --                          ] -- [offscreen] -- newStyle  
                --            ]
                --            style
                --    )
                --    model.styles
            } 
            ! [Cmd.none]
            
        Animate time ->
            ( { model
                | styles = List.map (Animation.update time) model.styles
              }
            , Cmd.none
            )

        Hovering (Over x) ->
            ( { model 
                | hovering = Just <| Over x
              }
            , Cmd.none
            )

        UnHover ->
            ( { model 
                | hovering = Nothing
              }
            , Cmd.none
            )

        Continue nextScreen ->
            case nextScreen of
                SelectColumn placeValue ->
                    { model 
                        | nextView = nextScreen
                        , sortPlace = Just placeValue
                    } 
                    ! [Cmd.none]
                _ ->
                    { model 
                        | nextView = nextScreen
                    }
                    ! [Cmd.none]

        SelectPlaceValueColumn placeValue columnNumber ->
            let
               --newStyles = pickupAnimationStyles model.styles
               newStyles = createStyle deckStyle
               animatedStyles = 
                    List.map4
                        (\i currentStyle intermediateAnimation finalAnimation ->
                            Animation.interrupt
                                [
                                 Animation.wait (toFloat i * 0.05 * Time.second)
                                ,Animation.to intermediateAnimation
                                ,Animation.wait (1 * Time.second)
                                ,Animation.to finalAnimation
                                ]
                                currentStyle 
                        )
                        (List.range 0 (List.length model.styles))
                        model.styles
                        deckStyle
                        cardDealtStyle
               --newModel = case placeValue of
               --             Units -> 
               --                 {model | hovering = Nothing
               --                         , unitColumn = Just columnNumber
               --                         , styles = newStyles
               --                         , deck = updateDeck placeValue targetBase3
               --                 }
                    
            in
            { model | hovering = Nothing
            , styles = animatedStyles --newStyles -- pickupAnimationStyles model.styles
             }
            ! [Cmd.none]
        --_ ->
            --{model | hovering = Nothing} ! [Cmd.none]



createStyle : List (List Animation.Property) -> List Animation.State
createStyle =
    List.map Animation.style 

-- Pickup Animation
{- 
The offset locations are dependent on the column
that the card is stored in
-}
pickupAnimationStyles : List Animation.State -> List Animation.State
pickupAnimationStyles styles =
    let
        (col1styles,col2styles,col3styles) = columnTuple styles
        cardOffset = 10 --0.06 * cardWIDTH 
        --(col1Offset,col2Offset,col3Offset) = 

        col1cards = List.indexedMap
                        (\i style ->
                            Animation.interrupt
                                [
                                 --Animation.wait (toFloat i * 0.5 )
                                 Animation.to 
                                    [
                                    Animation.translate 
                                        (px (toFloat i * 10))--(px ((toFloat i) * cardOffset)) 
                                        (px 0) -- ((toFloat i)*50))
                                    --Animation.x  0 
                                    --,Animation.y  0
                                    ]
                                ]
                                style
                        )
                        (col1styles ++ col2styles ++ col3styles) 

        col2cards = 
            List.map
                (\style -> --newStyle ->
                    Animation.interrupt
                        [ 
                          --Animation.wait (toFloat i * 0.05 * second)
                         Animation.to [Animation.translate (px -(Tuple.first boardCol2Offset)) (px 0)
                                      ] -- [offscreen] -- newStyle  
                        ]
                        style
                )
                col2styles    

                        
        col3cards = 
            List.map
                (\style -> --newStyle ->
                    Animation.interrupt
                        [ 
                          --Animation.wait (toFloat i * 0.05 * second)
                         Animation.to [Animation.translate (px 0) (px 400) --(px -(Tuple.first boardCol3Offset)) (px 0)
                                      ] -- [offscreen] -- newStyle  
                        ]
                        style
                )
                col3styles    

    in
        col1cards --++ col2cards ++ col3cards
            
    --List.map 
    --    (\style -> --newStyle ->
    --        Animation.interrupt
    --            [ 
    --              --Animation.wait (toFloat i * 0.05 * second)
    --             Animation.to [Animation.translate (px 100) (px 100)
    --                          ] -- [offscreen] -- newStyle  
    --            ]
    --            style
    --    )
    --    styles    



view : Model -> Html Msg
view model =
    case model.nextView of 
        Intro -> 
            showIntroScreen model
        ChooseNumber ->
            showChooseNumber model
        ChooseCard ->
            showChooseCard model
        SelectColumn placevalue ->
            showBoardDealtScreen model
    {-
    let 
        fillColor = case model.hovering of
                        Just (Over x) -> "blue"
                        _ -> "red"
        --cardSvg style =
        --    [Svg.g 
        --        ([
                --Svg.Events.onMouseOver (Msg "hovering")
                --,Svg.Events.onMouseOut (Msg "leaving")
        --        --,Svg.Events.onClick (PickupAll)
        --        ]
        --        ++
        --            (Animation.render <| Animation.style style )
        --        )
        --        [rect cardBg []
        --        ,Svg.text_ cardLabel [Svg.text "8"] 
        --        ]
        --    ]            

    in 
    svg
        [ version "1.1"
        , x "0"
        , y "0"
        , viewBox "0 0 400 400"
        ]    
        --<| List.map2
                --sortedCardView model.deck model.styles
        --[Svg.g [Svg.Events.onClick Pickup] [dealtCardView model.deck model.styles]]
        [Svg.g [Svg.Events.onClick Pickup] [dealtCardView model]]

-}

showIntroScreen : Model -> Html Msg
showIntroScreen model = 
    div 
        [] 
        [h1 
            [] 
            [Html.text "Let's see if I can guess your card..."]
        ,Html.button
            [Html.Events.onClick (Continue ChooseNumber)]
            [Html.text "Ok"]
        ]


showChooseNumber : Model -> Html Msg
showChooseNumber model =
    div
        []
        [
          Html.text "Choose a Number"
        , Html.button
            [Html.Events.onClick (Continue ChooseCard)]
            [Html.text "Ok"]
        ]


showChooseCard : Model -> Html Msg
showChooseCard model =
    div
        []
        [
          Html.text "Choose a Card"
        , Html.button
            [Html.Events.onClick (Continue <| SelectColumn Units)]
            [Html.text "Ok"]
        ]



showBoardDealtScreen : Model -> Html Msg
showBoardDealtScreen model = 
    let 
        fillColor = case model.hovering of
                        Just (Over x) -> "blue"
                        _ -> "red"
          

    in 
    svg
        [ version "1.1"
        , x "0"
        , y "0"
        , viewBox "0 0 400 400"
        ]    
        --<| List.map2
                --sortedCardView model.deck model.styles
        --[Svg.g [Svg.Events.onClick Pickup] [dealtCardView model.deck model.styles]]
        [Svg.g [] [dealtCardView model]]    
-- PREVIOUS!!        --[Svg.g [Svg.Events.onClick Pickup] [dealtCardView model]]    


subscriptions : Model -> Sub Msg
subscriptions model = 
    Animation.subscription 
        Animate
        model.styles

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
            --(\model ->
            --    Animation.subscription
            --        Animate
            --        model.styles
            --)
        }


-- CARD Styles
deck : List Card.Card
deck = 
    List.map
        (\i -> 
            case (rem i 4) of
                0 ->
                    Card.Card (Card.rank "clubs") i 
                1 -> 
                    Card.Card (Card.rank "hearts") i 
                2 -> 
                    Card.Card (Card.rank "spades") i 
                _ -> 
                    Card.Card (Card.rank "diamonds") i 
        )
        (List.range 1 27)

deckStyle : List (List Animation.Property)
deckStyle =
    let
        offset = 5
        take = 3
        yoffset = 10
        transProperty x y = 
            Animation.translate (px x) (px y)
    in
        List.map 
            (\i ->
                if i < take then
                    [transProperty (i * offset) yoffset]
                else
                    [transProperty (take * offset) yoffset] 
            ) 
            <| (List.map 
                    toFloat 
                    <| List.range 1 27 
                )



cardDealtStyle : List (List Animation.Property)
cardDealtStyle =
    let
        (rowOffset, colOffset) = (20,90)

        positions = List.map 
                        (\(x,y) -> 
                            (toFloat x, toFloat y) 
                        )
                        (List.map gridPos <| List.range 0 26 )

    in
        List.map 
            (\(x,y) ->
                [Animation.translate 
                    (px (colOffset * x)) 
                    (px (rowOffset * y))
                ]
            )
            positions
            

columnAnimationStyle : List (List Animation.Property)
columnAnimationStyle =
        let
        (rowOffset, colOffset) = (20,90)

        positions = List.map 
                        (\(x,y) -> 
                            (toFloat x, toFloat y) 
                        )
                        (List.map gridPos <| List.range 0 26 )

    in
        List.map 
            (\(x,y) ->
                [Animation.translate 
                    (px (colOffset * x))
                    (px (rowOffset * y))
                ]
                --[Animation.x (colOffset * x)
                --,Animation.y (rowOffset * y)
                --]
            )
            (
             --Debug.log 
                 --"positions" 
                 positions
            )

--[Animation.translate (px 0) (px 0)]

--layoutCards : List Card.Card -> List (List Animation.Property)
--layoutCards cards =
--    let
--        ar = 2.5 / 3.5
--        cardWidth = 50
--        cardHeight = cardWidth / ar
--        coloffset = 20 --cardWidth * 0.1
--        rowoffset =  cardHeight * 0.05
--        attr = 
--            [ Animation.attr "rx" 10 "px"
--            , Animation.attr "ry" 10 "px"
--            , Animation.width <| Animation.px cardWidth
--            , Animation.height <| Animation.px cardHeight
--            , Animation.fill Color.blue
--            ]

--        rows = 
--            List.map2 (,) 
--                (List.range 0 ((List.length cards) - 1)) 
--                (groupsOf 3 cards) 

--        offset (row,cs) =
--            let
--                cols = List.indexedMap 
--                        (\col card ->
--                            (toFloat col) * coloffset
--                        )
--                        cs

--                y = rowoffset * (toFloat row)
--                offsetAttr =
--                    List.map
--                        (\col ->
--                            [Animation.x col
--                            ,Animation.y y
--                            ]
--                        )
--                        cols
--            in
--                offsetAttr
--    in 
--       List.map 
--            ((++) attr )
--            <| List.concatMap offset rows

--sortedCardView : Card.Card -> Animation.State -> Html Msg  
--sortedCardView (Card.Card _ value) style =
--    let
--        ar = 2.5/3.5
--        cardWidth = 80.0
--        cardHeight = cardWidth / ar
--        bgProps = 
--            [
--              width <| toString cardWidth
--            , height <| toString cardHeight
--            , fill "orange"
--            , stroke "black"
--            , strokeWidth "2"
--            , rx "10"
--            , ry "10"
--            ]

--        labelProps =
--            [
--              stroke "black"
--            , fill "black"
--            , strokeWidth "1"
--            , x <| toString (cardWidth * 0.8)
--            , y <| toString (cardHeight * 0.2)
--            ]

--        attributes = 
--            --[Svg.Events.onClick Pickup]
--            --++
--            (Animation.render style)
--    in
--        Svg.g 
--            (attributes ++ [Svg.Events.onClick Pickup])
--            --((Animation.render cardstyle) 
--            --  ++ 
--            --  [
--            --    --Svg.Events.onMouseOver (Msg "hovering")
--            --  --, Svg.Events.onMouseOut (Msg "leaving")
--            --  Svg.Events.onClick Pickup
--            --  ]
--            --)

--            [ rect 
--                bgProps 
--                []
--            , Svg.text_ 
--                labelProps 
--                [Svg.text (toString value)]
--            ]
        
--dealtCardView : List Card.Card -> List (Animation.State) -> Svg.Svg Msg -- Html Msg
--dealtCardView cards styles =
dealtCardView : Model -> Svg.Svg Msg -- Html Msg
dealtCardView {deck,styles,hovering,sortPlace} = 
    let
        ar = 2.5/3.5
        cardWidth = 80.0
        cardHeight = cardWidth / ar

        (col1,col1style,bg1) =  (column1 deck, column1 styles, highlightStyle 1)  
        (col2,col2style,bg2) =  (column2 deck, column2 styles, highlightStyle 2)  
        (col3,col3style,bg3) =  (column3 deck, column3 styles, highlightStyle 3)  

        highlightStyle columnNum = 
            case hovering of
                Just (Over x) ->
                    if x == columnNum then
                        highlight x
                    else
                        rect [] []
                _ ->
                    rect [] []

        bgProps = 
            [
              width <| toString cardWidth
            , height <| toString cardHeight
            , fill "orange"
            , stroke "black"
            , strokeWidth "2"
            , rx "10"
            , ry "10"
            ]

        labelProps =
            [
              stroke "black"
            , fill "black"
            , strokeWidth "1"
            , x <| toString (cardWidth * 0.05)
            , y <| toString (cardHeight * 0.2)
            ]

        cardElem (Card.Card rank value) style = 
            Svg.g
                (Animation.render style)
                [rect bgProps []
                ,Svg.text_ labelProps [Svg.text (toString value)]
                ]

        highlight col = 
            let
                xoff = if col == 1 then
                            Tuple.first boardCol1Offset
                        else if col == 2 then
                            Tuple.first boardCol2Offset
                        else if col == 3 then
                            Tuple.first boardCol3Offset
                        else
                            0 
            in
                rect [x (toString xoff), width "90", height "300", stroke "blue", fill "red"] []


        onclickMsg = case sortPlace of
                        Just p ->
                            SelectPlaceValueColumn p
                        _ ->
                            always <| Msg "clicked on a column but I don't know what the place value to sort on should be. Check the model to ensure that the place value is set to a value when this screen is displayed."

        transformAttr (x,y) =
            transform 
                    <| "translate(" 
                        ++ (toString x)
                        ++ ","
                        ++ (toString y)
                        ++ ")" 


    in
        Svg.g 
            []
            [        
              bg1
            , bg2
            , bg3
            ,  Svg.g
                [
                --transformAttr boardCol1Offset
                (Svg.Events.onMouseOver <| Hovering (Over 1))
                ,(Svg.Events.onMouseOut UnHover)
                ,(Svg.Events.onClick <| onclickMsg 1)
                ]
                --<| [bg1]
                --    ++
                    (List.map2 cardElem col1 col1style)
                
            , Svg.g
                [
                 --transformAttr boardCol2Offset 
                (Svg.Events.onMouseOver <| Hovering (Over 2))
                ,(Svg.Events.onMouseOut UnHover)
                ,(Svg.Events.onClick <| onclickMsg 2)
                ]
                --<| [bg2]
                --    ++
                    (List.map2 cardElem col2 col2style)

            , Svg.g
                [
                 --transformAttr boardCol3Offset
                (Svg.Events.onMouseOver <| Hovering (Over 3))
                ,(Svg.Events.onMouseOut UnHover)
                ,(Svg.Events.onClick <| onclickMsg 3)
                ]
                --<| [bg3]
                --    ++
                    (List.map2 cardElem col3 col3style)                
            ]

           

deckView : List Card.Card -> List (Animation.State) -> Svg.Svg Msg
deckView cards styles =
    let
        ar = 2.5/3.5
        cardWidth = 80.0
        cardHeight = cardWidth / ar
        bgProps = 
            [
              width <| toString cardWidth
            , height <| toString cardHeight
            , fill "orange"
            , stroke "black"
            , strokeWidth "2"
            , rx "10"
            , ry "10"
            ]

        labelProps =
            [
              stroke "black"
            , fill "black"
            , strokeWidth "1"
            , x <| toString (cardWidth * 0.8)
            , y <| toString (cardHeight * 0.2)
            ]

        cardElem (Card.Card rank value) style = 
            Svg.g
                (Animation.render style)
                [rect bgProps []
                ,Svg.text_ labelProps [Svg.text (toString value)]
                ]

            
    in
        Svg.g
            []
            (List.map2 cardElem cards styles)    



cardBg : List (Svg.Attribute msg)
cardBg =
    let
        ar = 2.5/3.5
        cardWidth = 80.0
        cardHeight = cardWidth / ar
        --bg = rect bgProps []
        bgProps = 
            [
              width <| toString cardWidth
            , height <| toString cardHeight
            , fill "orange"
            , stroke "black"
            , strokeWidth "2"
            , rx "10"
            , ry "10"
            ]

    in    
        bgProps

cardLabel : List (Svg.Attribute msg)
cardLabel =
    let
        ar = 2.5/3.5
        cardWidth = 80.0
        cardHeight = cardWidth / ar

        labelProps =
            [
              stroke "black"
            , fill "black"
            , strokeWidth "1"
            , x <| toString (cardWidth * 0.8)
            , y <| toString (cardHeight * 0.2)
            ]
    in    
        labelProps

card =
    let
        ar = 2.5/3.5
        cardWidth = 80.0
        cardHeight = cardWidth / ar
        bgProps =
            [
              Animation.width (px cardWidth)
            , Animation.height (px cardHeight)
            , Animation.fill Color.orange
            , Animation.stroke Color.black
            , Animation.strokeWidth 3
            , Animation.custom "rx" 10 "px"
            , Animation.custom "ry" 10 "px"
            ]            

        labelProps =
            [ Animation.stroke Color.black
            , Animation.x (cardWidth * 0.8)
            , Animation.y (cardHeight * 0.2)
            ]

        props =
            let 
                (col,row) = gridPos 0
            in
                [ Animation.transformOrigin 
                    (px (toFloat col))
                    (px (toFloat row))
                    (px 0.0)
                ]

    in
        bgProps
            


cards : List (List Animation.Property)
cards =
    let
        ar = 2.5 / 3.5
        cardWidth = 50
        cardHeight = cardWidth / ar
        rowOffset = 30
        colOffset = cardWidth * 1.05
        positions = List.map gridPos <| List.range 0 26
        toAnimationProperty (col,row) = 
            [ Animation.x <| (colOffset * col)
            , Animation.y <| (rowOffset * row)
            , Animation.width (px cardWidth)
            , Animation.height (px cardHeight)
            , Animation.fill Color.orange
            , Animation.stroke Color.black
            , Animation.custom "rx" 10 "px"
            , Animation.custom "ry" 10 "px"
            ]
    in
        List.map 
            toAnimationProperty
            <| List.map (\(x,y) -> (toFloat x, toFloat y) ) positions
        --List.map toAnimationProperty positions

-- Coordinate Constants
boardCol1Offset = (0,0)
boardCol2Offset = (100,0)
boardCol3Offset = (200,0)

--cardDimensions 
cardWIDTH = 80.0



-- Utils

gridPos : Int -> (Int,Int)
gridPos i =
    let
        col = (flip rem) 3
        row = (flip (//)) 3
    in
    (col i, row i)


rowsOf3 = groupsOf 3
columns = transpose
column1 = rowsOf3 >> columns >> List.head >> (Maybe.withDefault [])
column2 = rowsOf3 >> columns >> List.drop 1 >> List.head >> (Maybe.withDefault [])
column3 = rowsOf3 >> columns >> last >> (Maybe.withDefault [])
columnTuple arr = (column1 arr, column2 arr, column3 arr)
