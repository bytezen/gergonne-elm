module Gergonne exposing (..)

import Html exposing (h1, div, Html)
import Html.Attributes as Attr
import Svg exposing (svg,rect)
import Svg.Attributes exposing (..)
import Svg.Events
import Color
import Animation exposing (px)
import List.Extra exposing (groupsOf, transpose, last)

import UI.Component.Card as Card



type Msg = 
    Msg String
    | Pickup
    | Animate Animation.Msg



type alias Model = 
   { 
     cards : List Card.Card
   , styles : List Animation.State
   , hovering : Bool
   }


init : ( Model, Cmd Msg )
init =
    {
     cards = deck
    --,styles = List.map (\props -> Animation.style props) cards
    ,styles = List.map (\props -> Animation.style props) [[Animation.translate (px 0) (px 0)]]
    ,hovering = False
    }
    ! [Cmd.none]


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Msg "hovering" ->
            {model | hovering = True} ! [Cmd.none]

        Pickup ->
            {model | styles =
                List.map 
                    (\style -> --newStyle ->
                        Animation.interrupt
                            [ 
                              --Animation.wait (toFloat i * 0.05 * second)
                             Animation.to [Animation.translate (px 100) (px 100)
                                          ] -- [offscreen] -- newStyle  
                            ]
                            style
                    )
                    model.styles
            } 
            ! [Cmd.none]
            
        Animate time ->
            ( { model
                | styles = List.map (Animation.update time) model.styles
              }
            , Cmd.none
            )

        _ ->
            {model | hovering = False} ! [Cmd.none]
    --case msg of
    --    UIMsg uimsg ->
    --        let
    --            (newuimodel, cmd ) =
    --                UI.update uimsg model.ui
    --        in
    --            {model | ui = newuimodel}
    --            !
    --            [Cmd.map UIMsg cmd]


    --    _ -> 
    --        (model, Cmd.none)


view : Model -> Html Msg
view model =
    let 
        fillColor = case model.hovering of
                        True -> "blue"
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
        --<| List.map (\s -> List.map cardSvg s) model.styles
        (List.map ( \cardstyle -> 
                        Svg.g 
                            ((Animation.render cardstyle) 
                              ++ 
                              [
                                --Svg.Events.onMouseOver (Msg "hovering")
                              --, Svg.Events.onMouseOut (Msg "leaving")
                              Svg.Events.onClick Pickup
                              ]
                            )

                            [ rect cardBg []
                            , Svg.text_ cardLabel [Svg.text "8"]
                            ]
                    ) 
                    model.styles
        )
        --[Svg.g 
        --    ([
        --    --Svg.Attributes.transform "translate(100,0)"
        --    Svg.Events.onMouseOver (Msg "hovering")
        --    ,Svg.Events.onMouseOut (Msg "leaving")
        --    --,Svg.Events.onClick (PickupAll)
        --    ]
        --    ++
        --    (Animation.render
        --    <| Animation.style [Animation.translate (px 100) (px 0) ]
        --    )
        --    )
        --    [rect cardBg []
        --    ,Svg.text_ cardLabel [Svg.text "8"] 
        --    ]
        --]


        --    rect
        --        [width "80"
        --        ,height "120"
        --        ,x "0"
        --        ,Svg.Attributes.rx "10"
        --        , fill fillColor --"red"
        --        ]
        --        []
        --    ,Svg.text_ 
        --        ([
        --        --fill "yellow"
        --         x "50"
        --        ,y "20"
        --        ]
        --        ++
        --        (Animation.render 
        --        <| Animation.style  [Animation.fill Color.yellow]
        --        )

        --        )
        --        [ Svg.text "8"]
        --    ]
        --]

        --[rect 
        --    [width "200", height "200", fill "red"]
        --    []
        --]

        --(List.map 
        --    (\cardstyle -> 
        --        rect (Animation.render cardstyle) []
        --    ) 
        --    model.styles
        --)


    --div [][]
    --Html.map UIMsg (UI.view model.ui)


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

layoutCards : List Card.Card -> List (List Animation.Property)
layoutCards cards =
    let
        ar = 2.5 / 3.5
        cardWidth = 50
        cardHeight = cardWidth / ar
        coloffset = 20 --cardWidth * 0.1
        rowoffset =  cardHeight * 0.05
        attr = 
            [ Animation.attr "rx" 10 "px"
            , Animation.attr "ry" 10 "px"
            , Animation.width <| Animation.px cardWidth
            , Animation.height <| Animation.px cardHeight
            , Animation.fill Color.blue
            ]

        rows = 
            List.map2 (,) 
                (List.range 0 ((List.length cards) - 1)) 
                (groupsOf 3 cards) 

        offset (row,cs) =
            let
                cols = List.indexedMap 
                        (\col card ->
                            (toFloat col) * coloffset
                        )
                        cs

                y = rowoffset * (toFloat row)
                offsetAttr =
                    List.map
                        (\col ->
                            [Animation.x col
                            ,Animation.y y
                            ]
                        )
                        cols
            in
                offsetAttr
    in 
        --[
        --[ Animation.x 10
        --    , Animation.y 40
        --    , Animation.width (px 80)
        --    , Animation.height (px 80)
        --    , Animation.fill Color.orange
        --    , Animation.stroke Color.black
        --    , Animation.custom "rx" 10 "px"
        --    , Animation.custom "ry" 10 "px"
        --    ]
        --]
        List.map 
            ((++) attr )
            <| List.concatMap offset rows

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
