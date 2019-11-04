port module Main exposing (main)

import Browser
import Html exposing (Html, h1, text)
import Json.Encode as E


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
    = SetActiveTab (Maybe Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetActiveTab id ->
            ( { model | activeTabId = id }, Cmd.none )



-- SUBSCRIPTIONS


port activeTab : (Maybe Int -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    activeTab (\id -> SetActiveTab id)



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
