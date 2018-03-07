module Buttons exposing (modal, next,increment,decrement,reload,restart)

import Html exposing (Html, button, span, text)
import Html.Attributes exposing (classList)
import Html.Events exposing (onClick)

modal : msg -> Html msg
modal =
    createButton "✘" 


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






--Html.button
--            [Html.Events.onClick (Continue ChooseNumber)]
--            [Html.text "⇧Ok"]

createButton : String -> msg -> Html msg
createButton label msg =
        button 
        ([onClick msg] ++ [classList w3Button])
        [text label]
            

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