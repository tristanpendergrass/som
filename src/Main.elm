port module Main exposing (main)

import Browser
import Html exposing (Html, button, div, form, input, li, span, text, ul)
import Html.Attributes exposing (checked, class, classList, disabled, placeholder, type_, value)
import Html.Events exposing (onCheck, onClick, onInput, onSubmit)
import Json.Decode as D
import Json.Encode as E
import List
import QueryParams
import Url exposing (Url)


main : Program ( String, D.Value ) Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


{-| Used to filter features by the input of the user
-}
matchString : String -> String -> Bool
matchString left right =
    String.contains (String.toLower left) (String.toLower right)


getDraftValue : DraftValue -> String
getDraftValue (DraftValue value) =
    value


getOriginalValue : OriginalValue -> String
getOriginalValue (OriginalValue value) =
    value



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
    , active : Bool
    }


type DraftValue
    = DraftValue String


type OriginalValue
    = OriginalValue String


type FeatureEditState
    = Editing Override DraftValue OriginalValue
    | NotEditing


type ActiveTab
    = MainTab
    | ArchiveTab


type alias Model =
    { nonce : Int
    , browserUrl : Maybe Url
    , overrides : List Override
    , archivedOverrides : List Override
    , feature : String
    , featureEditState : FeatureEditState
    , featureFilter : String
    , activeTab : ActiveTab
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
            D.map4 Override
                (D.field "id" D.int)
                (D.field "feature" D.string)
                (D.field "variantSelection" (D.map stringToVariantSelection D.string))
                (D.field "active" D.bool)

        overrides : List Override
        overrides =
            case D.decodeValue (D.field "overrides" (D.list overrideDecoder)) localStorageData of
                Ok val ->
                    val

                Err _ ->
                    []

        archivedOverrides : List Override
        archivedOverrides =
            case D.decodeValue (D.field "archivedOverrides" (D.list overrideDecoder)) localStorageData of
                Ok val ->
                    val

                Err _ ->
                    []
    in
    ( { nonce = nonce
      , browserUrl = Url.fromString initialBrowserUrl
      , overrides = overrides
      , archivedOverrides = archivedOverrides
      , feature = ""
      , featureEditState = NotEditing
      , featureFilter = ""
      , activeTab = MainTab
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
        overrideToJson { id, feature, variantSelection, active } =
            E.object
                [ ( "id", E.int id )
                , ( "feature", E.string feature )
                , ( "variantSelection", E.string <| Maybe.withDefault "" variantSelection )
                , ( "active", E.bool active )
                ]
    in
    E.object
        [ ( "nonce", E.int model.nonce )
        , ( "overrides", E.list overrideToJson model.overrides )
        , ( "archivedOverrides", E.list overrideToJson model.archivedOverrides )
        ]


applyOverridesToUrl : List Override -> Url -> Url
applyOverridesToUrl overrides oldUrl =
    let
        oldParams : List QueryParams.QueryParam
        oldParams =
            QueryParams.fromUrl oldUrl

        newParams : List QueryParams.QueryParam
        newParams =
            overrides
                |> List.filter .active
                |> List.foldl
                    (\override ->
                        \accumulator ->
                            case override.variantSelection of
                                Nothing ->
                                    accumulator

                                Just variant ->
                                    QueryParams.applyOverride override.feature variant accumulator
                    )
                    oldParams

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
    | HandleFeatureFilterInput String
    | ApplyOverrides
    | SetActiveTab ActiveTab
      -- Add Override
    | HandleAddOverrideFeatureInput String
    | HandleAddOverrideSubmit
      -- Edit Override
    | HandleVariantSelectionInput Override String
    | HandleOverrideActiveInput Override Bool
    | SetFeatureEdit (Maybe Override)
    | HandleFeatureDraftInput String
    | CancelFeatureEdit
    | Archive Override
    | Unarchive Override
    | Delete Override


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

        SetActiveTab activeTab ->
            ( { model | activeTab = activeTab }, Cmd.none )

        HandleAddOverrideFeatureInput feature ->
            ( { model | feature = feature }, Cmd.none )

        HandleAddOverrideSubmit ->
            let
                newOverride : Override
                newOverride =
                    Override model.nonce model.feature Nothing True

                newOverrides : List Override
                newOverrides =
                    newOverride :: model.overrides

                newModel : Model
                newModel =
                    { model | nonce = model.nonce + 1, overrides = newOverrides, feature = "" }
            in
            ( newModel, sendToLocalStorage <| encodeModel newModel )

        HandleOverrideActiveInput override active ->
            let
                newOverride : Override
                newOverride =
                    { override | active = active }

                newOverrides : List Override
                newOverrides =
                    replace override newOverride model.overrides

                newModel : Model
                newModel =
                    { model | overrides = newOverrides }
            in
            ( newModel, sendToLocalStorage <| encodeModel newModel )

        SetFeatureEdit maybeOverride ->
            case ( maybeOverride, model.featureEditState ) of
                ( Nothing, NotEditing ) ->
                    ( model, Cmd.none )

                ( Nothing, Editing previousOverride draftValue _ ) ->
                    ( { model
                        | featureEditState = NotEditing
                        , overrides = replace previousOverride { previousOverride | feature = getDraftValue draftValue } model.overrides
                      }
                    , Cmd.none
                    )

                ( Just override, NotEditing ) ->
                    ( { model | featureEditState = Editing override (DraftValue override.feature) (OriginalValue override.feature) }, Cmd.none )

                ( Just override, Editing previousOverride _ originalValue ) ->
                    ( { model
                        | featureEditState = Editing override (DraftValue override.feature) (OriginalValue override.feature)
                        , overrides = replace previousOverride { previousOverride | feature = getOriginalValue originalValue } model.overrides
                      }
                    , Cmd.none
                    )

        HandleFeatureDraftInput value ->
            case model.featureEditState of
                NotEditing ->
                    ( model, Cmd.none )

                Editing override _ originalValue ->
                    ( { model | featureEditState = Editing override (DraftValue value) originalValue }, Cmd.none )

        CancelFeatureEdit ->
            case model.featureEditState of
                NotEditing ->
                    ( model, Cmd.none )

                Editing override _ originalValue ->
                    ( { model
                        | featureEditState = NotEditing
                        , overrides = replace override { override | feature = getOriginalValue originalValue } model.overrides
                      }
                    , Cmd.none
                    )

        HandleFeatureFilterInput value ->
            ( { model | featureFilter = value }, Cmd.none )

        Archive override ->
            let
                newModel : Model
                newModel =
                    { model
                        | overrides = model.overrides |> List.filter ((/=) override)
                        , archivedOverrides = override :: model.archivedOverrides
                    }
            in
            ( newModel, sendToLocalStorage <| encodeModel newModel )

        Unarchive override ->
            let
                newModel : Model
                newModel =
                    { model
                        | overrides = override :: model.overrides
                        , archivedOverrides = model.archivedOverrides |> List.filter ((/=) override)
                    }
            in
            ( newModel, sendToLocalStorage <| encodeModel newModel )

        Delete override ->
            let
                newModel : Model
                newModel =
                    { model
                        | archivedOverrides = model.archivedOverrides |> List.filter ((/=) override)
                    }
            in
            ( newModel, sendToLocalStorage <| encodeModel newModel )



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
            [ button [ type_ "submit", disabled (model.feature == "") ] [ text "+" ]
            , input [ value model.feature, onInput HandleAddOverrideFeatureInput, placeholder "New Feature Name" ] []
            ]
        ]


renderOverride : FeatureEditState -> Override -> Html Msg
renderOverride featureEditState override =
    let
        variantValue : String
        variantValue =
            override.variantSelection |> Maybe.withDefault ""

        featureText : Html Msg
        featureText =
            span [] [ text override.feature ]

        labelOrInput : Html Msg
        labelOrInput =
            case featureEditState of
                NotEditing ->
                    featureText

                Editing editingOverride draftValue _ ->
                    if editingOverride == override then
                        input
                            [ value (getDraftValue draftValue)
                            , onInput HandleFeatureDraftInput
                            ]
                            []

                    else
                        featureText

        editOrCancelButton : Html Msg
        editOrCancelButton =
            case featureEditState of
                NotEditing ->
                    button [ onClick (SetFeatureEdit (Just override)) ] [ text "Edit" ]

                Editing editingOverride _ _ ->
                    if editingOverride == override then
                        span []
                            [ button [ onClick CancelFeatureEdit ] [ text "Cancel" ]
                            , button [ onClick (SetFeatureEdit Nothing) ] [ text "Confirm" ]
                            ]

                    else
                        button [ onClick (SetFeatureEdit (Just override)) ] [ text "Edit" ]
    in
    li []
        [ editOrCancelButton
        , labelOrInput
        , form [ class "variant-input", onSubmit ApplyOverrides ]
            [ input [ value variantValue, onInput (HandleVariantSelectionInput override) ] []
            ]
        , input [ type_ "checkbox", checked override.active, onCheck (HandleOverrideActiveInput override) ] []
        , button [ onClick (Archive override) ] [ text "Archive" ]
        ]


renderFeatureFilter : Model -> Html Msg
renderFeatureFilter model =
    input
        [ class "feature-filter"
        , onInput HandleFeatureFilterInput
        , value model.featureFilter
        , placeholder "Filter by feature name"
        ]
        []


renderTabs : Model -> Html Msg
renderTabs model =
    div [ class "tab-container" ]
        [ div
            [ class "tab"
            , classList [ ( "active", model.activeTab == MainTab ) ]
            , onClick (SetActiveTab MainTab)
            ]
            [ text "Main" ]
        , div
            [ class "tab"
            , classList [ ( "active", model.activeTab == ArchiveTab ) ]
            , onClick (SetActiveTab ArchiveTab)
            ]
            [ text "Archive" ]
        ]


renderArchivedOverride : Override -> Html Msg
renderArchivedOverride override =
    li []
        [ button [ onClick (Unarchive override) ] [ text "Unarchive" ]
        , button [ onClick (Delete override) ] [ text "Permanently Delete" ]
        , span [] [ text override.feature ]
        ]


view : Model -> Html Msg
view model =
    case model.activeTab of
        MainTab ->
            div []
                [ renderTabs model
                , renderFeatureFilter model
                , ul [ class "overrides" ]
                    (renderAddOverride model
                        :: (model.overrides
                                |> List.filter (.feature >> matchString model.featureFilter)
                                |> List.map (renderOverride model.featureEditState)
                           )
                    )
                , button
                    [ onClick ApplyOverrides
                    , disabled <| not <| List.any .active model.overrides
                    ]
                    [ text "Apply Overrides" ]
                ]

        ArchiveTab ->
            div []
                [ renderTabs model
                , ul []
                    (model.archivedOverrides
                        |> List.map renderArchivedOverride
                    )
                ]
