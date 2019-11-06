port module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, li, option, select, span, text, ul)
import Html.Attributes exposing (value)
import Html.Events exposing (onClick, onInput)
import Json.Encode as E
import List
import Set exposing (Set)
import String
import Task


main : Program String Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias Id =
    Int


type VariantSelection
    = NotSelected
    | Variant String


type alias Override =
    { id : Id
    , feature : String
    , variants : List String
    , selectedVariant : VariantSelection
    }


type alias Model =
    { nonce : Int
    , browserUrl : String
    , overrides : List Override
    }


init : String -> ( Model, Cmd Msg )
init initialBrowserUrl =
    ( { nonce = 100
      , browserUrl = initialBrowserUrl
      , overrides =
            [ { id = 0
              , feature = "foo"
              , variants = [ "OFF", "ON" ]
              , selectedVariant = NotSelected
              }
            ]
      }
    , Cmd.none
    )



-- UPDATE


port sendUrl : String -> Cmd msg


type Msg
    = SetBrowserUrl String
    | SendBrowserUrl String
    | HandleSelectedVariantInput Override String
    | ApplyOverrides


replace : a -> a -> List a -> List a
replace oldA newA =
    List.map
        (\ax ->
            if ax == oldA then
                newA

            else
                ax
        )


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
            ( { model | browserUrl = newUrl }, sendUrl newUrl )

        HandleSelectedVariantInput override selection ->
            let
                newOverride : Override
                newOverride =
                    { override
                        | selectedVariant =
                            if selection == "" then
                                NotSelected

                            else
                                Variant selection
                    }

                newOverrides : List Override
                newOverrides =
                    replace override newOverride model.overrides
            in
            ( { model | overrides = newOverrides }, Cmd.none )

        ApplyOverrides ->
            let
                reduceOverrides : Override -> List String -> List String
                reduceOverrides override accumulator =
                    case override.selectedVariant of
                        NotSelected ->
                            accumulator

                        Variant value ->
                            ("stormcrow_override=" ++ override.feature ++ ":" ++ value) :: accumulator

                parameters : List String
                parameters =
                    List.foldl reduceOverrides [] model.overrides

                newUrl : String
                newUrl =
                    model.browserUrl ++ "?" ++ String.join "&" parameters
            in
            ( model, sendUrl newUrl )



-- SUBSCRIPTIONS


port browserUrl : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    browserUrl (\url -> SetBrowserUrl url)



-- VIEW


renderOverride : Override -> Html Msg
renderOverride override =
    let
        renderOption : String -> Html Msg
        renderOption variant =
            option [ value variant ] [ text variant ]

        selectValue : String
        selectValue =
            case override.selectedVariant of
                NotSelected ->
                    ""

                Variant value ->
                    value
    in
    li []
        [ span [] [ text override.feature ]
        , select [ onInput (HandleSelectedVariantInput override), value selectValue ] (List.map renderOption ("" :: override.variants))
        , span [] [ text ("Selected: " ++ selectValue) ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text ("Url: " ++ model.browserUrl) ]
        , button [ onClick (SendBrowserUrl "&stormcrow_override=browse_rename_use_api_v2:ON") ] [ text "Set URL" ]
        , ul [] (List.map renderOverride model.overrides)
        , button [ onClick ApplyOverrides ] [ text "Apply Overrides" ]
        ]
