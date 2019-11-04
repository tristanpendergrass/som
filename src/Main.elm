module Main exposing (main)

import Browser
import Html exposing (Html, h1, text)


main : Program (Maybe Int) Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias Id =
    Int



-- type alias TabData =
--     { id : Id
--     , url : String
--     }


type alias Model =
    { activeTabId : Maybe Id

    -- , tabs : Dict Id TabData
    }


init : Maybe Int -> ( Model, Cmd Msg )
init initialActiveTabId =
    ( { activeTabId = initialActiveTabId }
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    let
        activeTabLabel : String
        activeTabLabel =
            case model.activeTabId of
                Nothing ->
                    "N/A"

                Just id ->
                    String.fromInt id
    in
    h1 [] [ text ("Active Tab ID: " ++ activeTabLabel) ]
