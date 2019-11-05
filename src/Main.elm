port module Main exposing (main)

import Browser
import Html exposing (Html, h1, text)
import Json.Encode as E


main : Program String Model Msg
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
    { browserUrl : String

    -- , tabs : Dict Id TabData
    }


init : String -> ( Model, Cmd Msg )
init initialBrowserUrl =
    ( { browserUrl = initialBrowserUrl }
    , Cmd.none
    )



-- UPDATE


type Msg
    = SetBrowserUrl String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetBrowserUrl url ->
            ( { model | browserUrl = url }, Cmd.none )



-- SUBSCRIPTIONS


port browserUrl : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    browserUrl (\url -> SetBrowserUrl url)



-- VIEW


view : Model -> Html Msg
view model =
    h1 [] [ text ("Url: " ++ model.browserUrl) ]
