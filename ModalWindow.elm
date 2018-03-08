module Modal exposing (window, Modal (..) )

import Html exposing (Html, div, span, text)
import Html.Attributes exposing (classList)
import Html.Events exposing (onClick)

import Buttons exposing (modal)


type Modal a = Modal String Boolean a


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
                [text copy]

        containerDiv =
            div
                [
                  class "w3-container"
                , class "w3-red"
                ]

                [
                  Buttons.modal
                , copycontent 
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
        ,style displayStyle
        ]
        [
          contentDiv
        ]


<div class="w3-modal" style="display: block;">
    <div class="w3-modal-content">
        <div class="w3-container w3-red">
            <span class="w3-button w3-display-topright _w3-large w3-circle w3-xlarge _w3-ripple w3-black" >
                ✘
            </span>
            <div><h1>Instructions for the thing</h1>
            </div>
        </div>
    </div>
</div>


modal 