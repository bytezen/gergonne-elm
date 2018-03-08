module Modal exposing (window,selectPileModal, Modal (..) )

import Html exposing (Html, div, span, text, p)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)

import Buttons exposing (modal)


type Modal a = Modal String Bool a


window : Modal msg -> Html msg 
window (Modal copy isopen msg) =
    let
        displayStyle =
            if isopen then
                ("display", "block")
            else
                ("display", "none")

        copyDiv =
            div
                []
                [Html.h2 [] [text copy] ]

        containerDiv =
            div
                [
                  class "w3-container"
                , class "w3-red"
                ]

                [
                 copyDiv 
                , Buttons.modal msg
                ]

        contentDiv =
            div
                [
                  class "w3-modal-content"
                ]

                [ containerDiv ]

    in
            
    div 
        [class "w3-modal"
        ,style [displayStyle,("min-height","800px")]
        ]
        [
          contentDiv
        ]

selectPileModal : Bool -> msg -> Html msg
selectPileModal isopen msg =
    let
        copyDiv = 
            div
                []
                [
                 p [] [Html.text "PAY CLOSE ATTENTION !! "]
                ,p [] [Html.text "I am going to place images into 3 piles."]
                ,p [] [Html.text "You have to remember the pile where I layed your SECRET educator."]
                ,p [] [Html.text "When I am done placing the cards click the pile where your educator was placed."]
                ,p [] [Html.text "We are going to do this 3 times..."]
                -- ]
                ]


        displayStyle =
            if isopen then
                ("display", "block")
            else
                ("display", "none")

        containerDiv =
            div
                [
                  class "w3-container"
                , class "w3-red"
                ]

                [
                 copyDiv 
                , Buttons.modal msg
                ]

        contentDiv =
            div
                [
                  class "w3-modal-content"
                ]

                [ containerDiv ]
            
    in
        div 
            [class "w3-modal"
            ,style [displayStyle,("min-height","800px")]
            ]
            [
              contentDiv
            ]    
        --Modal 
        --    selectPileCopy 
        --    model.modalModel.showSelectPile 
        --    (Continue SelectColumn) 

