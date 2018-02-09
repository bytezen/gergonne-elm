module UI.UI exposing (..)

import UI.SortPlace as SortPlace
import Html exposing (Html,div,text)
import Html.Events exposing (onClick)

import Animation
import Color


type alias Column = Int

type Msg = 
     SortPlaceMsg SortPlace.InternalMsg
    | Select Column
    | Animate Animation.Msg


type alias Model = 
    {
    sortPlace : SortPlace.Model
    , count : Int
    , styles : List (Animation.Property)
    --selectedColumns : Maybe (List Int)
    }


-- Translators for the child components
sortPlaceTranslator : SortPlace.Translator Msg
sortPlaceTranslator =
    SortPlace.translator { 
                           onInternalMsg = SortPlaceMsg 
                         , onColumnSelect= Select 
                         }


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of 
        SortPlaceMsg sortPlaceInternal ->
            let
                ( sortPlaceNewModel, cmd ) =
                    SortPlace.update sortPlaceInternal model.sortPlace
            in
                { model | sortPlace = sortPlaceNewModel } 
                ! [ Cmd.map sortPlaceTranslator cmd ]
                    
        Select c ->
            let 
                newstyles = List.map (\s -> Animation.interrupt 
                                [Animation.to 
                                    [Animation.custom "rx" 50 "px"] -- Color.green]
                                ]
                                s
                                )
                                model.sortPlace.styles
                spmodel = model.sortPlace
                newSortPlaceModel = { spmodel | styles = newstyles }
            in
            --({model | count = c}, Cmd.none)
            ({ model | count = c
             , sortPlace = newSortPlaceModel
             }
             , Cmd.none)

        Animate time ->
            let
                spmodel = model.sortPlace
                updateStyles = List.map (Animation.update time) spmodel.styles
                newSortPlaceModel = { spmodel | styles = updateStyles}
            in
                    
            ({ model | sortPlace = newSortPlaceModel }, Cmd.none )
            --(model, Cmd.none)
        --Animate time ->
        --    ( { model
        --        | styles = List.map (Animation.update time) model.styles
        --      }
        --    , Cmd.none
        --    )

subscription : Model -> Sub msg
subscription model =
    Sub.none

--handleChildComponentMsg : ChildrenMsg -> Model -> (Model, Cmd Msg)
--handleChildComponentMsg msg model =
--    case msg of
--        SortPlace spMsg ->
--            let
--                ( sortPlaceNewModel, cmd ) =
--                    SortPlace.update spMsg model.sortPlace
--            in
--                { model | sortPlace = sortPlaceNewModel } 
--                ! [ Cmd.map sortPlaceTranslator cmd ]            


--handleUIMsg : InternalMsg -> Model -> (Model, Cmd Msg)
--handleUIMsg msg model =
--    case msg of
--        Select i ->
--            Debug.log 
--                ("selected column: " ++ (toString i) )
--                ( model , Cmd.none)

view : Model -> Html Msg
view model =
    -- view should switch based on current UI screen
    --Html.map sortPlaceTranslator (SortPlace.view model.sortPlace) 
    div [] 
    [text (toString model.count)
    , Html.map sortPlaceTranslator (SortPlace.view model.sortPlace) 
    ]


init : Model 
init =
    {
     sortPlace = SortPlace.init
    ,count = 0
    ,styles = []
    }