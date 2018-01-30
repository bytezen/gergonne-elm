module App exposing (..)

import Html exposing (h1, div, Html)
import Html.Attributes as Attr

import UI.UI as UI 

type Msg = 
    UIMsg UI.Msg
    | Msg

type alias Model = 
   { ui : UI.Model
   , clickCount : Int
   }


init : ( Model, Cmd Msg )
init =
    ( { 
       ui = UI.init
       ,clickCount = 0
      }
      , Cmd.none
    )


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        UIMsg uimsg ->
            let
                (newuimodel, cmd ) =
                    UI.update uimsg model.ui
            in
                {model | ui = newuimodel}
                !
                [Cmd.map UIMsg cmd]
        _ -> 
            (model, Cmd.none)


view : Model -> Html Msg
view model =
    Html.map UIMsg (UI.view model.ui)


subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

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
