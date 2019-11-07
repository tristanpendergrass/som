port module Main exposing (main)

import Browser
import Html exposing (Html, button, div, li, option, select, span, text, ul)
import Html.Attributes exposing (value)
import Html.Events exposing (onClick, onInput)
import List
import Regex
import String
import Url exposing (Url)


main : Program String Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


colon : Regex.Regex
colon =
    Maybe.withDefault Regex.never <|
        Regex.fromString ":|%3A"


queryParamsFromUrl : Url -> List QueryParam
queryParamsFromUrl url =
    case url.query of
        Nothing ->
            []

        Just query ->
            query
                |> String.split "&"
                |> List.map queryParamFromString


{-| Turn a string into a QueryParam.

  - Will make it a StormcrowParam if it fits the format. e.g. "stormcrow\_override=foo:bar"
  - Will make it an OtherParam if it does not fit. e.g. "shtormcrow\_override=foo:bar", "stormcrow\_override=foobar", "bar=baz"

-}
queryParamFromString : String -> QueryParam
queryParamFromString param =
    if String.startsWith "stormcrow_override=" param then
        case
            param
                |> String.dropLeft 19
                |> Regex.split colon
        of
            [] ->
                OtherParam param

            [ feature, variant ] ->
                StormcrowParam feature variant

            _ ->
                OtherParam param

    else
        OtherParam param


stringFromQueryParam : QueryParam -> String
stringFromQueryParam param =
    case param of
        OtherParam value ->
            value

        StormcrowParam feature variant ->
            "stormcrow_override=" ++ feature ++ ":" ++ variant


{-| Iterate over a list of query params and either modify the params matching the override or
append a new StormcrowParam at the end if no param matched the override

    e.g.

    applyOverride
        { feature = Feature "foo", selectedVariant = Variant "bar" }
        [ OtherParam "other=value", StormcrowParam "foo" "baz" ]

    -- equals [ OtherParam "other=value", StormcrowParam "foo" "bar" ]

    -- OR

    applyOverride
        { feature = Feature "foo", selectedVariant = Variant "bar" }
        [ OtherParam "other=value", StormcrowParam "baz" "bing" ]

    -- equals [ OtherParam "other=value", StormcrowParam "baz" "bing", StormcrowParam "foo" "bar" ]

-}
applyOverride : Override -> List QueryParam -> List QueryParam
applyOverride override queryParams =
    case override.selectedVariant of
        EmptySelection ->
            queryParams

        VariantSelection variant ->
            let
                ( overridePresent, newQueryParams ) =
                    List.foldr
                        (\queryParam ->
                            \( found, accQueryParams ) ->
                                case queryParam of
                                    OtherParam _ ->
                                        ( found, queryParam :: accQueryParams )

                                    StormcrowParam feature _ ->
                                        if feature == override.feature then
                                            ( True, StormcrowParam feature variant :: accQueryParams )

                                        else
                                            ( found, queryParam :: accQueryParams )
                        )
                        ( False, [] )
                        queryParams
            in
            if overridePresent then
                newQueryParams

            else
                List.append queryParams [ StormcrowParam override.feature variant ]


queryStringFromQueryParams : List QueryParam -> Maybe String
queryStringFromQueryParams queryParams =
    case queryParams of
        [] ->
            Nothing

        params ->
            params
                |> List.map stringFromQueryParam
                |> String.join "&"
                |> Just


applyOverridesToUrl : List Override -> Url -> Url
applyOverridesToUrl overrides oldUrl =
    let
        oldParams : List QueryParam
        oldParams =
            queryParamsFromUrl oldUrl

        newParams : List QueryParam
        newParams =
            List.foldl applyOverride oldParams overrides

        newUrl : Url
        newUrl =
            { oldUrl | query = queryStringFromQueryParams newParams }
    in
    newUrl



-- MODEL


type alias Id =
    Int


type alias Feature =
    String


type alias Variant =
    String


type QueryParam
    = StormcrowParam Feature Variant
    | OtherParam String


type VariantSelection
    = EmptySelection
    | VariantSelection Variant


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
              , selectedVariant = EmptySelection
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
                                EmptySelection

                            else
                                VariantSelection selection
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
                EmptySelection ->
                    ""

                VariantSelection value ->
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
