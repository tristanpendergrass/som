port module Main exposing (main)

import Browser
import Html exposing (Html, button, div, li, option, select, span, text, ul)
import Html.Attributes exposing (value)
import Html.Events exposing (onClick, onInput)
import List
import QueryParams
import Url exposing (Url)


main : Program String Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias Id =
    Int


type alias VariantSelection =
    Maybe QueryParams.Variant


type alias Override =
    { id : Id
    , feature : String
    , variants : List String
    , selectedVariant : VariantSelection
    }


type alias Model =
    { nonce : Int
    , browserUrl : Maybe Url
    , overrides : List Override
    }


init : String -> ( Model, Cmd Msg )
init initialBrowserUrl =
    ( { nonce = 100
      , browserUrl = Url.fromString initialBrowserUrl
      , overrides =
            [ { id = 0
              , feature = "foo"
              , variants = [ "OFF", "ON" ]
              , selectedVariant = Nothing
              }
            ]
      }
    , Cmd.none
    )



-- UPDATE


port sendUrl : String -> Cmd msg


type Msg
    = SetBrowserUrl String
    | HandleSelectedVariantInput Override String
    | ApplyOverrides


applyOverridesToUrl : List Override -> Url -> Url
applyOverridesToUrl overrides oldUrl =
    let
        oldParams : List QueryParams.QueryParam
        oldParams =
            QueryParams.fromUrl oldUrl

        newParams : List QueryParams.QueryParam
        newParams =
            List.foldl
                (\override ->
                    \accumulator ->
                        case override.selectedVariant of
                            Nothing ->
                                accumulator

                            Just variant ->
                                QueryParams.applyOverride override.feature variant accumulator
                )
                oldParams
                overrides

        newUrl : Url
        newUrl =
            { oldUrl | query = QueryParams.toString newParams }
    in
    newUrl


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
            ( { model | browserUrl = Url.fromString url }, Cmd.none )

        HandleSelectedVariantInput override selection ->
            let
                newOverride : Override
                newOverride =
                    { override
                        | selectedVariant =
                            if selection == "" then
                                Nothing

                            else
                                Just selection
                    }

                newOverrides : List Override
                newOverrides =
                    replace override newOverride model.overrides
            in
            ( { model | overrides = newOverrides }, Cmd.none )

        ApplyOverrides ->
            case model.browserUrl of
                Nothing ->
                    ( model, Cmd.none )

                Just oldUrl ->
                    let
                        newUrl : Url
                        newUrl =
                            applyOverridesToUrl model.overrides oldUrl
                    in
                    ( model, sendUrl <| Url.toString newUrl )



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
                Nothing ->
                    ""

                Just value ->
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
        [ ul [] (List.map renderOverride model.overrides)
        , button [ onClick ApplyOverrides ] [ text "Apply Overrides" ]
        ]
