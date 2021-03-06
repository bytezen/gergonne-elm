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
import CardFaces 
import Buttons
import Modal exposing (Modal(..))

type alias CardColumn = Int

type HoverState = Over CardColumn

type PlaceValue = Units | Threes | Nines

type Base3Digit = Zero | One | Two

type Base3 = Base3 Base3Digit Base3Digit Base3Digit

type ModalId = 
      IntroModal
    | ChooseCardModal
    | ChooseNumberModal
    | SelectPileModal 

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
    | Shuffle (Array (Card.Card String))
    | IncrementTarget
    | DecrementTarget
    | CloseModal ModalId
    | ShowModal ModalId
    --| FisherYates Int

type ScreenId =
      ChooseCard
    | ChooseNumber
    | Intro
    | SelectColumn 
    | Guess


type alias CardModel =
    {
      width : Int
    , height : Int
    , bgProps : List (Svg.Attribute Msg)
    , labelProps : List (Svg.Attribute Msg)
    }

type alias ModalModel =
    {
      showIntro : Bool
    , showChooseNumber : Bool
    , showChooseCard : Bool 
    , showSelectPile : Bool
    }

type alias Model = 
   { 
     deck : List (Card.Card String)
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
   --, showModal : Bool
   , modalModel : ModalModel
   }


init : ( Model, Cmd Msg )
init =
    {
     deck = deck
    ,styles = createStyle offscreenStyle --faceUpAllStyle -- columnAnimationStyle
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
    --,showModal = False
    ,modalModel = initModalModel
    }
    ! [(shuffle deck)]--[Cmd.map (always Shuffle) Cmd.none]


initModalModel : ModalModel
initModalModel =
    {
    showIntro = True
    ,showChooseCard = True
    ,showChooseNumber = True
    ,showSelectPile = False
    }


closeModal : ModalModel
closeModal =
    initModalModel

setModal : ModalId -> Bool -> ModalModel -> ModalModel
setModal id show model =
            case id of
                IntroModal ->
                    {model | showIntro = show} 
                ChooseNumberModal->
                    {model | showChooseNumber = show }
                ChooseCardModal ->
                    {model | showChooseCard = show} 
                SelectPileModal ->
                    {model | showSelectPile = show} 

toggleModalModel : ModalId -> ModalModel -> ModalModel
toggleModalModel id ({showIntro, showChooseCard, showChooseNumber, showSelectPile} as model) =
                    model

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ShowModal id ->
            {model | modalModel = setModal id True model.modalModel}
            ! [Cmd.none]

        CloseModal id ->
            {model | modalModel = setModal id False model.modalModel}
            ! [Cmd.none]            
            --let
                --modalModel = model.modalModel
                --newModalModel = setModal id False model.modalModel
                    --case id of
                    --    IntroModal ->
                    --        {modalModel | showIntro = False} 
                    --    ChooseNumberModal->
                    --        {modalModel | showChooseNumber = False }
                    --    ChooseCardModal ->
                    --        {modalModel | showChooseCard = False} 
                    --    SelectPileModal ->
                    --        {modalModel | showSelectPile = False} 
            --in
            --    {model | modalModel = newModalModel } ! [Cmd.none]



        IncrementTarget ->
            let
                target_ = case model.target of
                            Nothing ->
                                13
                            Just x ->
                                x
                target = clamp 1 27 (target_ + 1)
            in
                    
                {model |
                      target = Just target
                    , targetBase3 = Just <| toBase3 (target - 1)
                }
                ! [Cmd.none]


        DecrementTarget ->
            let
                target_ = case model.target of
                            Nothing ->
                                13
                            Just x ->
                                x
                target = clamp 1 27 (target_ - 1)
            in
                    
                {model |
                      target = Just target
                    , targetBase3 = Just <| toBase3 (target - 1)
                }
                ! [Cmd.none]
                    


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
                    let
                       animatedStyles = 
                            List.map4
                                (\i currentStyle intermediateStyle finalStyle ->
                                    Animation.interrupt
                                        [
                                         Animation.wait (toFloat i * 0.01 * Time.second)
                                        , Animation.to intermediateStyle 
                                        , Animation.wait (toFloat i * 0.5 * Time.second)
                                        , Animation.to finalStyle
                                        --, Animation.wait (toFloat 1 * Time.second)
                                        --, Animation.Messenger.send AnimationOver
                                        ]
                                        currentStyle
                                )
                                (List.range 0 (List.length model.styles))
                                model.styles
                                offscreenStyle
                                threePileStyle   
                            
                    in
                            
                    { model 
                        | nextView = nextScreen
                        , sortPlace = Just Units
                        , styles = animatedStyles
                        , isAnimating = True
                        , modalModel = setModal SelectPileModal False model.modalModel
                        --, showModal = True
                    } 
                    ! [Cmd.none]

                ChooseNumber ->
                    { model | nextView = nextScreen}
                     ! [Random.generate RandomTarget (Random.int 0 26)]

                ChooseCard ->
                    let
                        animatedStyles = 
                            List.map3
                                (\i currentStyle finalAnimation ->
                                    Animation.interrupt
                                        [
                                         Animation.wait (toFloat i * 0.01 * Time.second)
                                        , Animation.to finalAnimation
                                        , Animation.wait (toFloat 1 * Time.second)
                                        , Animation.Messenger.send AnimationOver
                                        ]
                                        currentStyle
                                )
                                (List.range 0 (List.length model.styles))
                                model.styles
                                faceUpAllStyle
                    in
                            
                        { model 
                            | nextView = nextScreen
                            , styles = animatedStyles
                            , isAnimating = True
                            --, showModal = True
                        }
                        ! [Cmd.none]
                _ ->
                    {model | nextView = nextScreen} ! [Cmd.none]


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
                                 Animation.wait (toFloat i * 0.01 * Time.second)
                                ,Animation.to intermediateAnimation
                                ,Animation.wait (toFloat i * 0.50 * Time.second)
                                ,Animation.to finalAnimation
                                ,Animation.wait ( 27 * Time.second) --let animation finish before message is sent
                                ,Animation.Messenger.send AnimationOver
                                ]
                                currentStyle 
                        )
                        (List.range 0 (List.length model.styles))
                        model.styles
                        offscreenStyle --deckStyle
                        threePileStyle


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

                applyToDeck : (List (Card.Card String) -> List (Card.Card String) ) -> List (Card.Card String)
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


shuffle : List (Card.Card String) -> Cmd Msg
shuffle cards =
    let
        --generator = Random.Array.shuffle (Array.fromList cards) 
        createCard (CardFaces.Shuffle urlArray) =
            Array.indexedMap 
                (\i url -> 
                    Card.Card (Card.rank url) i
                )
                urlArray
            |> Array.slice 0 27
 
    in
        --Random.generate Shuffle generator 
        Cmd.map (createCard >> Shuffle) CardFaces.faces            


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
            [Html.text "Can computers read minds ??? "]
        , Buttons.next (Continue ChooseNumber)
        --,Html.button
        --    [Html.Events.onClick (Continue ChooseNumber)]
        --    [Html.text "⇧Ok"]
        ]


showChooseNumber : Model -> Html Msg
showChooseNumber model =
    let

        copy = "Choose a number between 1 and 27. When you are done click the number button."

        showModal = model.modalModel.showChooseNumber

        modal = Modal copy showModal (CloseModal ChooseNumberModal)

        classes = 
            ["w3-button"
            , "w3-large"
            , "w3-circle"
            , "w3-xlarge"
            , "w3-ripple"
            , "w3-black"
            ]
        btnClasses = List.map 
                        (\s -> (s,True))
                        classes


        target = case model.target of
                    Nothing ->
                        0
                    Just x ->
                        x
    in
            
    div
        --([Attr.style 
        --    [("backgroundColor", "red")]
        -- ]
        -- ++
        -- [Attr.align "center"
        -- ]
        -- ) 
        []
        ([
           Modal.window modal
         , h1 [] [Html.text "Choose a Number"]
         , div [class "w3-section"] [Buttons.increment IncrementTarget]
         , div 
            [class "w3-section" ] [Buttons.squareLabelled(toString target) (Continue ChooseCard)]
            --[Html.button
            --    [Html.Events.onClick (Continue ChooseCard)]
            --    [Html.text (toString target)]
            --]
         , div [class "w3-section" ] [ Buttons.decrement DecrementTarget]
         ]
        --++
        --[svg 
        --    [x "0", y "0"] 
        --    [ rect 
        --        [x "0", width "100", height "100", fill "yellow", stroke "red"] []
            
        --    ]
        --]
        )


showChooseCard : Model -> Html Msg
showChooseCard model =
    let

        chooseCardCopy = "SECRETLY choose a favorite Center Educator...Then click the Next button."
        --Find a favorite educator, BUT DO NOT CLICK ON THEMChoose your favorite CEE educator..."
        selectPileCopy = "PAY CLOSE ATTENTION !! I am going to place images into 3 piles. You have to remember the pile where I layed your SECRET educator. When I am done placing the cards click the pile where your educator was placed." 

        --showChooseCardModal = model.modalModel.showChooseCard --model.showModal && (not model.isAnimating)

        chooseCardModal = 
            Modal 
                chooseCardCopy 
                model.modalModel.showChooseCard 
                (CloseModal ChooseCardModal) 


        selectPileModal = 
            Modal.selectPileModal 
                model.modalModel.showSelectPile
                (Continue SelectColumn)  

        svgElem {deck, styles} =
            List.map2
                cardElem
                deck
                styles

        cardElem (Card.Card (Card.Rank url) value) style = 
            Svg.g
                (Animation.render style)
                [
                 rect cardModel.bgProps []
                ,Svg.text_ 
                    cardModel.labelProps 
                    [Svg.text (toString value)
                    ]
                ,Svg.image 
                    [
                     xlinkHref url
                    ,width (toString cardModel.width)
                    ,height (toString cardModel.height)
                    ] 
                    []
                ]
            
    in
            
    div
        []
        [
          Modal.window chooseCardModal
        , selectPileModal
        , svg
            [width boardWidth, height boardHeight]
            --[width "800", height "600"]
            (svgElem model)

        , Buttons.next (ShowModal SelectPileModal) --(Continue <| SelectColumn )
        ]


--svgButton : String -> msg -> {b|wt:String, ht:String, xp: String, yp:String} -> Svg.Svg msg
--svgButton label msg {xp, yp, wt, ht} =
--    let
--        bg = rect [x xp, y yp, width wt, height ht, fill "black"] []
--        lbl = Svg.text_ [x xp, y "20", stroke "yellow"] [Svg.text label]
--        --Svg.text_ labelProps [Svg.text (toString value)]
            
--    in
            
--    Svg.g 
--        [Svg.Events.onClick msg]
--        [bg,lbl]

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
                , Attr.style 
                    [
                    ("width", boardWidth)
                    ,("height", boardHeight)
                    --,("width", "800px")
                    --,("width", "800px")
                    ]
                ]    
                [Svg.g [] [dealtCardView model]]    
        -- PREVIOUS!!        --[Svg.g [Svg.Events.onClick Pickup] [dealtCardView model]]    

        showModal = model.modalModel.showSelectPile --model.showModal && (not model.isAnimating)

        modal = Modal copy showModal (Continue <| SelectColumn ) --(CloseModal SelectPileModal) 

        copy = "PAY CLOSE ATTENTION !! I am going to place images into 3 piles. You have to remember the pile where I layed your SECRET educator. When I am done placing the cards click the pile where your educator was placed." 


    in 
        div 
            []
            [
             --Modal.window modal
             --Html.h3 [] [Html.text <| toString model.isAnimating]
             svgElem
            ]


showGuess : Model -> Html Msg
showGuess model = 
    let
        target = case model.targetBase3 of
                    Just n -> 
                          fromBase3 n
                    _ ->
                        0

        guess = Maybe.withDefault 
                    (Card.Card (Card.rank "foo") 0 )
                    ( model.deck !! target )
                

        cardElem (Card.Card (Card.Rank url) value) = 
            Svg.g
                --(Animation.render style)
                [ transform "translate(250,30)" ]
                [
                 rect cardModel.bgProps []
                ,Svg.text_ cardModel.labelProps [Svg.text (toString value)]
                ,Svg.image 
                    [
                     xlinkHref url
                    ,width "320" --<| toString cardModel.width
                    ,height "400" --<| toString cardModel.height
                    ] 
                    []
                ]        
            
    in
            
        div []
            [ 
              h1 [] 
                [Html.text "Your educator is ..." ]
            , 
            svg 
                [width boardWidth, height boardHeight]
                --[] 
                [cardElem guess]
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
deck : List (Card.Card String)
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



offscreenStyle : List (List Animation.Property)
offscreenStyle =
    let
        offset = 5
        take = 3
        yoffset = 10
        transProperty x y = 
            Animation.translate (px x) (px y)

        translateOffscreen =
            Animation.translate (px 300) (px -500)
    in
        List.map 
            --(\i ->
            --    if i < take then
            --        [transProperty (i * offset) yoffset]
            --    else
            --        [transProperty (take * offset) yoffset] 
            --) 
            (always [translateOffscreen])
            <| (List.map 
                    toFloat 
                    <| List.range 1 27 
                )


deckStyle : List (List Animation.Property)
deckStyle =
    let
        offset = 5
        take = 3
        yoffset = 10
        transProperty x y = 
            Animation.translate (px x) (px y)

        translateOffscreen =
            Animation.translate (px -100) (px -100)
    in
        List.map 
            --(\i ->
            --    if i < take then
            --        [transProperty (i * offset) yoffset]
            --    else
            --        [transProperty (take * offset) yoffset] 
            --) 
            (always [translateOffscreen])
            <| (List.map 
                    toFloat 
                    <| List.range 1 27 
                )

faceUpAllStyle : List (List Animation.Property)
faceUpAllStyle =
    let
        colCount = 7

        colPadding = 1.15 * toFloat cardModel.width

        rowPadding = 1.10 * toFloat cardModel.height

        toPosition i =
            ( rem i colCount, i // colCount ) 

        positions =
            List.map toPosition (List.range 0 26)

    in
        List.map 
            (\ (x,y) ->
                [Animation.translate
                    (px ( toFloat x * colPadding))
                    (px ( toFloat y * rowPadding))
                ]
            )
            positions


threePileStyle : List (List Animation.Property)
threePileStyle =
    let
        (rowOffset, colOffset) = (20,pileColumnOffset)

        positions = List.map 
                        (\(x,y) -> 
                            (toFloat x, toFloat y) 
                        )
                        (List.map gridPos <| List.range 0 26 )

    in
        List.map 
            (\(x,y) ->
                [Animation.translate 
                    (px (colOffset * x )) 
                    (px (rowOffset * 1))
                    --(px (rowOffset * y))
                ]
            )
            positions
            

threeColumnStyle : List (List Animation.Property)
threeColumnStyle =
    let
        (rowOffset, colOffset) = (20,190)

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
                    (px (rowOffset * 1))
                    --(px (rowOffset * y))
                ]
            )
            positions
 
columnAnimationStyle : List (List Animation.Property)
columnAnimationStyle =
        let
        (rowOffset, colOffset) = (20,200) --(20,90)

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
        --ar = 2.5/3.5
        --cardWidth = 80.0
        --cardHeight = cardWidth / ar

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
              width <| toString cardModel.width
            , height <| toString cardModel.height
            , fill "orange"
            , stroke "black"
            , strokeWidth "2"
            , rx "1"
            , ry "1"
            ]

        labelProps =
            [
              stroke "black"
            , fill "black"
            , strokeWidth "1"
            , x <| toString (toFloat cardModel.width * 0.05)
            , y <| toString (toFloat cardModel.height * 0.2)
            ]

        -- card display element

        cardElem (Card.Card (Card.Rank url) value) style = 
            Svg.g
                (Animation.render style)
                [
                 rect bgProps []
                --,Svg.text_ labelProps [Svg.text (toString value)]
                ,Svg.image 
                    [
                     xlinkHref url
                    ,width <| toString cardModel.width 
                    ,height <| toString cardModel.height 
                    ] 
                    []
                ]

        highlight col = 
            let
                xoff = if col == 1 then
                            0
                            --Tuple.first boardCol1Offset
                        else if col == 2 then
                            pileColumnOffset
                            --Tuple.first boardCol2Offset
                        else if col == 3 then
                            pileColumnOffset * 2
                            --Tuple.first boardCol3Offset
                        else
                            0 

                padding =  toFloat cardModel.width * 0.1 
            in
                rect 
                    [x (toString (xoff - padding))
                    , y (toString (20.0 - padding))
                    , width (toString (toFloat cardModel.width + (2.0 * padding)))
                    , height (toString (toFloat cardModel.height + (2.0 * padding)))
                    , stroke "blue"
                    , fill "red"
                    ] 
                    []


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
            [ transformAttr (200,100)]
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

           

deckView : List (Card.Card String)-> List (Animation.State) -> Svg.Svg Msg
deckView cards styles =
    let
        ar = 2.5/3.5
        cardWidth = 80.0
        cardHeight = cardWidth / ar
        bgProps = 
            [
              width <| toString cardWidth
            , height <| toString cardHeight
            --, fill "orange"
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
            --, fill "orange"
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

--card =
--    let
--        ar = 2.5/3.5
--        cardWidth = 80.0
--        cardHeight = cardWidth / ar
--        bgProps =
--            [
--              Animation.width (px cardWidth)
--            , Animation.height (px cardHeight)
--            --, Animation.fill Color.orange
--            , Animation.stroke Color.black
--            , Animation.strokeWidth 3
--            , Animation.custom "rx" 10 "px"
--            , Animation.custom "ry" 10 "px"
--            ]            

--        labelProps =
--            [ Animation.stroke Color.black
--            , Animation.x (cardWidth * 0.8)
--            , Animation.y (cardHeight * 0.2)
--            ]

--        props =
--            let 
--                (col,row) = gridPos 0
--            in
--                [ Animation.transformOrigin 
--                    (px (toFloat col))
--                    (px (toFloat row))
--                    (px 0.0)
--                ]

--    in
--        bgProps
            


--cards : List (List Animation.Property)
--cards =
--    let
--        ar = 2.5 / 3.5
--        cardWidth = 50
--        cardHeight = cardWidth / ar
--        rowOffset = 30
--        colOffset = cardWidth * 1.05
--        positions = List.map gridPos <| List.range 0 26
--        toAnimationProperty (col,row) = 
--            [ Animation.x <| (colOffset * col)
--            , Animation.y <| (rowOffset * row)
--            , Animation.width (px cardWidth)
--            , Animation.height (px cardHeight)
--            --, Animation.fill Color.orange
--            , Animation.stroke Color.black
--            , Animation.custom "rx" 10 "px"
--            , Animation.custom "ry" 10 "px"
--            ]
--    in
--        List.map 
--            toAnimationProperty
--            <| List.map (\(x,y) -> (toFloat x, toFloat y) ) positions

-- Coordinate Constants
boardCol1Offset = (0,0)
boardCol2Offset = (100,0)
boardCol3Offset = (200,0)

--cardDimensions 
cardWIDTH = 150 --80
cardAR = 640 / 800.0 -- 2.5 / 3.5
cardHEIGHT = 188 --100 --floor ( (toFloat cardWIDTH) / cardAR )

cardModel : CardModel
cardModel = 
    {
      width = cardWIDTH
    , height = cardHEIGHT
    , bgProps = 
        [
          width <| toString cardWIDTH
        , height <| toString cardHEIGHT
        , fill "blue"
        , stroke "black"
        , strokeWidth "2"
        , rx "1"
        , ry "1"
        ]
    , labelProps =
        [
          stroke "black"
        , fill "black"
        , strokeWidth "1"
        , x <| toString ( toFloat cardWIDTH * 0.05)
        , y <| toString ( toFloat cardHEIGHT * 0.2)
        ]

    }


-- LAYOUT MODEL
pileColumnOffset = 225 --190

boardWidth = toString 1300
boardHeight = toString 800

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

fromBase3Digit : Base3Digit -> Int
fromBase3Digit digit =
    case digit of
        One -> 1
        Two -> 2
        _ -> 0

fromBase3 : Base3 -> Int
fromBase3 (Base3 n t u) =
    9 * (fromBase3Digit n) + 3 * ( fromBase3Digit t) + (fromBase3Digit u)


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

cardValue : Card.Card a -> Int
cardValue (Card.Card _ value) = value



