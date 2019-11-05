port module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, text)
import Html.Events exposing (onClick)
import Json.Encode as E


main : Program String Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias Model =
    { browserUrl : String
    }


init : String -> ( Model, Cmd Msg )
init initialBrowserUrl =
    ( { browserUrl = initialBrowserUrl }
    , Cmd.none
    )



-- UPDATE


port sendUrl : String -> Cmd msg


type Msg
    = SetBrowserUrl String
    | SendBrowserUrl String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetBrowserUrl url ->
            ( { model | browserUrl = url }, Cmd.none )

        SendBrowserUrl url ->
            let
                newUrl : String
                newUrl =
                    model.browserUrl ++ url
            in
            ( { model | browserUrl = newUrl }, sendUrl <| newUrl )



-- SUBSCRIPTIONS


port browserUrl : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    browserUrl (\url -> SetBrowserUrl url)



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text ("Url: " ++ model.browserUrl) ]
        , button [ onClick (SendBrowserUrl "&stormcrow_override=browse_rename_use_api_v2:ON") ] [ text "Set URL" ]
        ]
