port module Main exposing (main)

import Browser
import Html exposing (Html, button, div, form, input, li, span, text, ul)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import List
import QueryParams
import Url exposing (Url)


main : Program String Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias Id =
    Int


type alias Feature =
    String


type alias Variant =
    String


type alias VariantSelection =
    Maybe Variant


type alias Override =
    { id : Id
    , feature : Feature
    , variants : List Variant
    , selectedVariant : VariantSelection
    }


type alias Model =
    { nonce : Int
    , browserUrl : Maybe Url
    , overrides : List Override
    , feature : String
    }


defaultVariants : List Variant
defaultVariants =
    [ "OFF", "ON" ]


init : String -> ( Model, Cmd Msg )
init initialBrowserUrl =
    ( { nonce = 100
      , browserUrl = Url.fromString initialBrowserUrl
      , overrides =
            [ { id = 0
              , feature = "foo"
              , variants = defaultVariants
              , selectedVariant = Nothing
              }
            ]
      , feature = ""
      }
    , Cmd.none
    )



-- UPDATE


port sendUrl : String -> Cmd msg


type Msg
    = SetBrowserUrl String
    | HandleSelectedVariantInput Override String
    | ApplyOverrides
      -- Add Override
    | HandleAddOverrideFeatureInput String
    | HandleAddOverrideSubmit


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

        HandleAddOverrideFeatureInput feature ->
            ( { model | feature = feature }, Cmd.none )

        HandleAddOverrideSubmit ->
            let
                newOverride : Override
                newOverride =
                    Override model.nonce model.feature defaultVariants Nothing

                newOverrides : List Override
                newOverrides =
                    newOverride :: model.overrides
            in
            ( { model | nonce = model.nonce + 1, overrides = newOverrides, feature = "" }, Cmd.none )



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
    li []
        [ span [] [ text override.feature ]
        , form [ onSubmit ApplyOverrides ]
            [ input [ onInput (HandleSelectedVariantInput override) ] []
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
        ]
