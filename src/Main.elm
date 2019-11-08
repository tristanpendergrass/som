port module Main exposing (main)

import Browser
import Html exposing (Html, button, div, form, input, li, span, text, ul)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Json.Decode as D
import Json.Encode as E
import List
import QueryParams
import Url exposing (Url)


main : Program ( String, D.Value ) Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias Id =
    Int


type alias Feature =
    String


type alias Variant =
    String


type alias Override =
    { id : Id
    , feature : Feature
    , variantSelection : Maybe Variant
    }


type alias Model =
    { nonce : Int
    , browserUrl : Maybe Url
    , overrides : List Override
    , feature : String
    }


init : ( String, D.Value ) -> ( Model, Cmd Msg )
init ( initialBrowserUrl, localStorageData ) =
    let
        nonce : Int
        nonce =
            case D.decodeValue (D.field "nonce" D.int) localStorageData of
                Ok val ->
                    val

                Err _ ->
                    100

        stringToVariantSelection : String -> Maybe Variant
        stringToVariantSelection value =
            if value == "" then
                Nothing

            else
                Just value

        overrideDecoder : D.Decoder Override
        overrideDecoder =
            D.map3 Override
                (D.field "id" D.int)
                (D.field "feature" D.string)
                (D.field "variantSelection" (D.map stringToVariantSelection D.string))

        overrides : List Override
        overrides =
            case D.decodeValue (D.field "overrides" (D.list overrideDecoder)) localStorageData of
                Ok val ->
                    val

                Err _ ->
                    []
    in
    ( { nonce = nonce
      , browserUrl = Url.fromString initialBrowserUrl
      , overrides = overrides
      , feature = ""
      }
    , Cmd.none
    )



-- UPDATE


port sendUrl : String -> Cmd msg


port sendToLocalStorage : E.Value -> Cmd msg


encodeModel : Model -> E.Value
encodeModel model =
    let
        overrideToJson : Override -> E.Value
        overrideToJson { id, feature, variantSelection } =
            E.object
                [ ( "id", E.int id )
                , ( "feature", E.string feature )
                , ( "variantSelection", E.string <| Maybe.withDefault "" variantSelection )
                ]
    in
    E.object
        [ ( "nonce", E.int model.nonce )
        , ( "overrides", E.list overrideToJson model.overrides )
        ]


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
                        case override.variantSelection of
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


type Msg
    = SetBrowserUrl String
    | HandleVariantSelectionInput Override String
    | ApplyOverrides
      -- Add Override
    | HandleAddOverrideFeatureInput String
    | HandleAddOverrideSubmit


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetBrowserUrl url ->
            ( { model | browserUrl = Url.fromString url }, Cmd.none )

        HandleVariantSelectionInput override selection ->
            let
                newOverride : Override
                newOverride =
                    { override
                        | variantSelection =
                            if selection == "" then
                                Nothing

                            else
                                Just selection
                    }

                newOverrides : List Override
                newOverrides =
                    replace override newOverride model.overrides

                newModel : Model
                newModel =
                    { model | overrides = newOverrides }
            in
            ( newModel, sendToLocalStorage <| encodeModel newModel )

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

        HandleAddOverrideFeatureInput feature ->
            ( { model | feature = feature }, Cmd.none )

        HandleAddOverrideSubmit ->
            let
                newOverride : Override
                newOverride =
                    Override model.nonce model.feature Nothing

                newOverrides : List Override
                newOverrides =
                    newOverride :: model.overrides

                newModel : Model
                newModel =
                    { model | nonce = model.nonce + 1, overrides = newOverrides, feature = "" }
            in
            ( newModel, sendToLocalStorage <| encodeModel model )



-- SUBSCRIPTIONS


port browserUrl : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    browserUrl (\url -> SetBrowserUrl url)



-- VIEW


renderAddOverride : Model -> Html Msg
renderAddOverride model =
    li []
        [ form [ onSubmit HandleAddOverrideSubmit ]
            [ button [ type_ "submit" ] [ text "+" ]
            , input [ value model.feature, onInput HandleAddOverrideFeatureInput ] []
            ]
        ]


renderOverride : Override -> Html Msg
renderOverride override =
    let
        variantValue : String
        variantValue =
            override.variantSelection |> Maybe.withDefault ""
    in
    li []
        [ span [] [ text override.feature ]
        , form [ onSubmit ApplyOverrides ]
            [ input [ value variantValue, onInput (HandleVariantSelectionInput override) ] []
            ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ ul []
            (renderAddOverride model
                :: List.map renderOverride model.overrides
            )
        , button [ onClick ApplyOverrides ] [ text "Apply Overrides" ]
        , div [] [ text ("nonce:" ++ String.fromInt model.nonce) ]
        ]
