module Buttons exposing (modal, next,increment,decrement,reload,restart, labelled, squareLabelled)

import Html exposing (Html, button, span, text)
import Html.Attributes exposing (classList)
import Html.Events exposing (onClick)

modal : msg -> Html msg
modal msg =
    button 
        ([onClick msg] ++ [classList w3ModalButton])
        [text "got it"]   
    --createButton "got it" --"✘" 


next : msg -> Html msg
next =
    createButton "⇰"

increment : msg -> Html msg
increment =
    createButton "⇧"

decrement : msg -> Html msg
decrement =
    createButton "⇩"


reload : msg -> Html msg
reload =
    createButton "↺"


restart: msg -> Html msg
restart =
    createButton "↩"

labelled : String -> msg -> Html msg
labelled = createButton

squareLabelled : String -> msg -> Html msg 
squareLabelled = createSquareButton



--Html.button
--            [Html.Events.onClick (Continue ChooseNumber)]
--            [Html.text "⇧Ok"]

createButton : String -> msg -> Html msg
createButton label msg =
        button 
        ([onClick msg] ++ [classList w3Button])
        [text label]


createSquareButton : String -> msg -> Html msg
createSquareButton label msg = 
        button 
        ([onClick msg] ++ [classList w3SquareButton])
        [text label]    
            


w3ModalButton : List (String, Bool)
w3ModalButton =
    let
        classes = 
            ["w3-btn"
            , "w3-round-xlarge"
            , "w3-white"
            , "w3-border"
            , "w3-border-blue"
            , "w3-white"
            ]

        btnClasses = List.map 
                        (\s -> (s,True))
                        classes    
    in
        btnClasses            


w3SquareButton : List (String, Bool)
w3SquareButton =
    let
        classes = 
            ["w3-button"
            , "w3-large"
            --, "w3-circle"
            , "w3-xlarge"
            , "w3-ripple"
            , "w3-black"
            ]

        btnClasses = List.map 
                        (\s -> (s,True))
                        classes    
    in
        btnClasses            

w3Button : List (String, Bool)
w3Button =
    let
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
    in
        btnClasses