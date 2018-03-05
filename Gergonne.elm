module Gergonne exposing (..)

import Html exposing (h1,h3, div, Html)
import Html.Attributes as Attr
import Html.Events
import Svg exposing (svg,rect)
import Svg.Attributes exposing (..)
import Svg.Events
import Color
import Animation exposing (px)
import Animation.Messenger
import List.Extra exposing (groupsOf, transpose, last)
import Time
import Random 
import List.Extra exposing ((!!))
import Random.Array
import Array exposing (Array)

import UI.Component.Card as Card


type alias CardColumn = Int

type HoverState = Over CardColumn

type PlaceValue = Units | Threes | Nines

type Base3Digit = Zero | One | Two

type Base3 = Base3 Base3Digit Base3Digit Base3Digit


type Msg = 
      Msg String
    | AnimationOver
    | Pickup
    | Hovering HoverState
    | UnHover
    | Animate Animation.Msg
    | Continue ScreenId
    | SelectPlaceValueColumn PlaceValue CardColumn
    | RandomTarget Int
    | Shuffle (Array Card.Card)
    --| FisherYates Int

type ScreenId =
      ChooseCard
    | ChooseNumber
    | Intro
    | SelectColumn 
    | Guess


type alias Model = 
   { 
     deck : List Card.Card
   , styles : List (Animation.Messenger.State Msg) --List Animation.State
   , hovering : Maybe HoverState
   , nextView : ScreenId
   , target : Maybe Int
   , targetBase3 : Maybe Base3
   , sortPlace : Maybe PlaceValue
   , messages : Maybe String
   , unitColumn: Maybe CardColumn
   , threesColumn : Maybe CardColumn
   , ninesColumn : Maybe CardColumn
   , isAnimating : Bool
   }


init : ( Model, Cmd Msg )
init =
    {
     deck = deck
    ,styles = createStyle columnAnimationStyle
    ,hovering = Nothing
    ,nextView = Intro
    ,target = Nothing
    ,targetBase3 = Base3 Zero One Two |> Just
    ,sortPlace = Nothing
    ,messages = Nothing
    ,unitColumn = Nothing
    ,threesColumn = Nothing
    ,ninesColumn = Nothing
    ,isAnimating = False
    }
    ! [(shuffle deck)]--[Cmd.map (always Shuffle) Cmd.none]


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Shuffle shuffledDeck ->
            {model | 
                deck = Array.toList shuffledDeck 
            } 
            ! [Cmd.none]

        RandomTarget i ->
            { model | 
                target = Just i
                , targetBase3 = toBase3 i |> Just}
            ! [Cmd.none]

        Msg s ->
            {model | messages = Just s} ! [Cmd.none]

        Pickup ->
            {model | styles =  
                    --List.map 
                    --(\props -> Animation.style props) 
                    --deckStyle
                pickupAnimationStyles model.styles
            } 
            ! [Cmd.none]
            
        Animate animMsg ->
            let
                updateTuples = --[(styles, cmd)]
                    List.map (Animation.Messenger.update animMsg) model.styles
                    
            in
                    
             { model
                | styles = 
                    List.map (Tuple.first) updateTuples
                    --List.map (Animation.update animMsg) model.styles
              }
            ! List.map (Tuple.second)  updateTuples
               --[Cmd.none]
            

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
                SelectColumn ->
                    { model 
                        | nextView = nextScreen
                        , sortPlace = Just Units
                    } 
                    ! [Cmd.none]

                ChooseNumber ->
                    { model | nextView = nextScreen}
                     ! [Random.generate RandomTarget (Random.int 0 26)]

                _ ->
                    { model 
                        | nextView = nextScreen
                    }
                    ! [Cmd.none]

        AnimationOver ->
            {model |
                isAnimating = False
                } ! [Cmd.none]

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
                                ,Animation.wait ( 2 * Time.second) --let animation finish before message is sent
                                ,Animation.Messenger.send AnimationOver
                                ]
                                currentStyle 
                        )
                        (List.range 0 (List.length model.styles))
                        model.styles
                        deckStyle
                        cardDealtStyle


                deckGenerators : Base3 ->  List ( List a -> List a )
                deckGenerators targetBase3 =
                        case digit placeValue targetBase3 of
                            Zero ->
                                if columnNumber == 1 then
                                    column1Top
                                else if columnNumber == 2 then
                                    column2Top
                                else if columnNumber == 3 then
                                    column3Top
                                else
                                    Debug.log 
                                        ("bad columnNumber: " ++ (toString columnNumber)
                                            ++ "returning column1,column2,column3 as order")
                                        column1Top
                            One ->
                                if columnNumber == 1 then
                                    column1Middle
                                else if columnNumber == 2 then
                                    column2Middle
                                else if columnNumber == 3 then
                                    column3Middle
                                else
                                    Debug.log 
                                        ("bad columnNumber: " ++ (toString columnNumber)
                                            ++ "returning column2,column1,column3 as order")
                                        column1Middle

                            Two ->
                                if columnNumber == 1 then
                                    column1Bottom
                                else if columnNumber == 2 then
                                    column2Bottom
                                else if columnNumber == 3 then
                                    column3Bottom
                                else
                                    Debug.log 
                                        ("bad columnNumber: " ++ (toString columnNumber)
                                            ++ "returning column3,column2,column1 as order")
                                        column1Top

                applyToDeck : (List Card.Card -> List Card.Card) -> List Card.Card
                applyToDeck = (|>) model.deck

                newDeck = 
                    case model.targetBase3 of
                        Just targetBase3 ->
                            List.concatMap applyToDeck (deckGenerators targetBase3)
                        _ -> 
                            model.deck

                    
                nextPlaceValue : Maybe PlaceValue
                nextPlaceValue =
                    case placeValue of
                        Units ->
                            Just Threes
                        Threes ->
                            Just Nines
                        _ ->
                            Nothing

                nextScreen : ScreenId
                nextScreen =
                    case placeValue of
                        Nines ->
                            Guess
                        _ ->
                            SelectColumn

            in
            { model | hovering = Nothing
            , styles = animatedStyles --newStyles -- pickupAnimationStyles model.styles
            , deck = newDeck
            , sortPlace = nextPlaceValue
            , nextView = nextScreen
            , isAnimating = True
             }
            ! [Cmd.none]
        --_ ->
            --{model | hovering = Nothing} ! [Cmd.none]


shuffle : List Card.Card -> Cmd Msg
shuffle cards =
    let
        generator = Random.Array.shuffle (Array.fromList cards) 
    in
        Random.generate Shuffle generator 


--createStyle : List (List Animation.Property) -> List Animation.State
createStyle : List (List Animation.Property) -> List (Animation.Messenger.State Msg)
createStyle =
    List.map Animation.style 

-- Pickup Animation
{- 
The offset locations are dependent on the column
that the card is stored in
-}
--pickupAnimationStyles : List Animation.State -> List Animation.State
pickupAnimationStyles : List (Animation.Messenger.State Msg) -> List (Animation.Messenger.State Msg)
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
            



view : Model -> Html Msg
view model =
    case model.nextView of 
        Intro -> 
            showIntroScreen model
        ChooseNumber ->
            showChooseNumber model
        ChooseCard ->
            showChooseCard model
        SelectColumn ->
            showBoardDealtScreen model
        Guess ->
            showGuess model


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
    let
        btn = 
            svgButton "testing" 
                (Continue ChooseCard) 
                {xp = "0", yp="0", wt = "100", ht = "100"}
            
    in
            
    div
        ([Attr.style 
            [("backgroundColor", "red")]
         ]
         ++
         [Attr.align "center"
         ]
         ) 

        ([
          Html.p [] [Html.text "Choose a Number"]
        , Html.button
            [Html.Events.onClick (Continue ChooseCard)]
            [Html.text "Ok"]
        ]
        ++
        [svg 
            [x "0", y "0"] 
            [ rect 
                [x "0", width "100", height "100", fill "yellow", stroke "red"] []
            , btn
            ]
        ]
        )


showChooseCard : Model -> Html Msg
showChooseCard model =
    div
        []
        [
          Html.text "Choose a Card"
        , Html.button
            [Html.Events.onClick (Continue <| SelectColumn )]
            [Html.text "Ok"]
        ]


svgButton : String -> msg -> {b|wt:String, ht:String, xp: String, yp:String} -> Svg.Svg msg
svgButton label msg {xp, yp, wt, ht} =
    let
        bg = rect [x xp, y yp, width wt, height ht, fill "black"] []
        lbl = Svg.text_ [x xp, y "20", stroke "yellow"] [Svg.text label]
        --Svg.text_ labelProps [Svg.text (toString value)]
            
    in
            
    Svg.g 
        [Svg.Events.onClick msg]
        [bg,lbl]

showBoardDealtScreen : Model -> Html Msg
showBoardDealtScreen model = 
    let 
        fillColor = case model.hovering of
                        Just (Over x) -> "blue"
                        _ -> "red"
        svgElem =    
            svg
                [ version "1.1"
                , x "0"
                , y "0"
                --, viewBox "0 0 400 400"
                , Attr.style 
                [
                --    ("padding", "50px 30px")
                ("height", "600px")]
                ]    
                [Svg.g [] [dealtCardView model]]    
        -- PREVIOUS!!        --[Svg.g [Svg.Events.onClick Pickup] [dealtCardView model]]    
    in 
        div 
            []
            [
            Html.h3 [] [Html.text <| toString model.isAnimating]
            , svgElem
            ]


showGuess : Model -> Html Msg
showGuess model = 
    let
        target = case model.target of
                    Just n -> 
                        n
                    _ ->
                        0

        guess = Maybe.withDefault 
                    (Card.Card (Card.rank "foo") 0 )
                    ( model.deck !! target )
                |> cardValue
            
    in
            
    div []
        [ h1 [] 
            [Html.text "Your Card was..." ]
        , h3 [] [Html.text <| toString guess]
        ]

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

-- Hacky brute force first pass of column stacking alogrithm
column1Top : List ( List a -> List a)
column1Top  = [column1 , column2 , column3 ]
column1Middle  = [column2 , column1 , column3 ]
column1Bottom  = [column3 , column2 , column1 ]

column2Top  = [column2 , column1 , column3 ]
column2Middle  = [column1 , column2 , column3 ]
column2Bottom  = [column1 , column3 , column2 ]

column3Top  = [column3 , column2 , column1 ]
column3Middle  = [column1 , column3 , column2 ]
column3Bottom  = [column1 , column2 , column3 ]


digit : PlaceValue -> Base3 -> Base3Digit
digit p (Base3 n t u) =
    case p of 
        Nines ->
            n
        Threes -> 
            t
        Units ->
            u

toBase3Digit : Int -> Base3Digit
toBase3Digit n =
    case n of
        1 -> One
        2 -> Two
        _ -> Zero


toBase3 : Int -> Base3
toBase3 n =
    let
        toBase3_ n accum =
            if n <= 0 then
                accum
            else
                toBase3_ (n // 3) ([rem n 3] ++ accum )

        digits = List.map
                    toBase3Digit
            
    in

            
    if n > 26 then
        Base3 Zero Zero Zero
    else
        case digits <| toBase3_ n [] of
            u :: [] ->
                Base3 Zero Zero u
            t :: u :: [] ->
                Base3 Zero t u
            n :: t :: u :: [] ->
                Base3 n t u 
            _ ->
                Base3 Zero Zero Zero

remainders x = 
    List.foldl 
        (\_ (n,rs) ->
            (n // 3, rs ++ [rem n 3])
            )
        (x,[])
        (List.range 1 3)

cardValue : Card.Card -> Int
cardValue (Card.Card _ value) = value
            

            




