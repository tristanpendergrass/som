port module Main exposing (main)

import Browser
import Browser.Dom
import FeatherIcons
import Html exposing (Html, button, div, form, h1, h2, input, option, select, text)
import Html.Attributes exposing (checked, class, classList, disabled, id, placeholder, selected, style, type_, value)
import Html.Events exposing (onBlur, onCheck, onClick, onInput, onSubmit)
import Json.Decode as D
import Json.Encode as E
import List
import QueryParams exposing (QueryParam)
import Task
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


variantSelectionToString : VariantSelection -> String
variantSelectionToString value =
    case value of
        OnVariant ->
            "OnVariant"

        OffVariant ->
            "OffVariant"

        CustomVariant ->
            "CustomVariant"


variantSelectionFromString : String -> VariantSelection
variantSelectionFromString value =
    case value of
        "OnVariant" ->
            OnVariant

        "OffVariant" ->
            OffVariant

        "CustomVariant" ->
            CustomVariant

        _ ->
            OffVariant



-- MODEL


type alias Id =
    Int


type alias Feature =
    String


type VariantSelection
    = OnVariant
    | OffVariant
    | CustomVariant


type alias Override =
    { id : Id
    , feature : Feature
    , variantSelection : VariantSelection
    , customVariantText : String
    , isSelected : Bool
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

        overrideDecoder : D.Decoder Override
        overrideDecoder =
            D.map5 Override
                (D.field "id" D.int)
                (D.field "feature" D.string)
                (D.field "variantSelection" (D.map variantSelectionFromString D.string))
                (D.field "customVariantText" D.string)
                (D.field "isSelected" D.bool)

        overrides : List Override
        overrides =
            let
                overrideSorter : Override -> Override -> Order
                overrideSorter left right =
                    if left.isSelected == right.isSelected then
                        EQ

                    else if right.isSelected then
                        GT

                    else
                        LT
            in
            D.decodeValue (D.field "overrides" (D.list overrideDecoder)) localStorageData
                |> Result.map (List.sortWith overrideSorter)
                |> Result.withDefault []

        archivedOverrides : List Override
        archivedOverrides =
            D.decodeValue (D.field "archivedOverrides" (D.list overrideDecoder)) localStorageData
                |> Result.withDefault []
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


port createTab : String -> Cmd msg


encodeModel : Model -> E.Value
encodeModel model =
    let
        overrideToJson : Override -> E.Value
        overrideToJson { id, feature, variantSelection, customVariantText, isSelected } =
            E.object
                [ ( "id", E.int id )
                , ( "feature", E.string feature )
                , ( "variantSelection", E.string (variantSelectionToString variantSelection) )
                , ( "customVariantText", E.string customVariantText )
                , ( "isSelected", E.bool isSelected )
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
        oldParams : List QueryParam
        oldParams =
            QueryParams.fromUrl oldUrl

        newParams : List QueryParam
        newParams =
            overrides
                |> List.filter .isSelected
                |> List.foldl
                    (\override ->
                        \accumulator ->
                            case override.variantSelection of
                                OnVariant ->
                                    QueryParams.applyOverride override.feature "ON" accumulator

                                OffVariant ->
                                    QueryParams.applyOverride override.feature "OFF" accumulator

                                CustomVariant ->
                                    QueryParams.applyOverride override.feature override.customVariantText accumulator
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
    | FocusResult (Result Browser.Dom.Error ())
    | OpenGithub
      -- Add Override
    | HandleAddOverrideFeatureInput String
    | HandleAddOverrideSubmit
      -- Edit Override
    | HandleVariantSelectionInput Override String
    | HandleCustomVariantInput Override String
    | SetFeatureEdit (Maybe Override)
    | HandleFeatureDraftInput String
    | CancelFeatureEdit
    | ToggleSelectOverride Override Bool
    | Archive Override
    | Unarchive Override
    | Delete Override


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetBrowserUrl url ->
            ( { model | browserUrl = Url.fromString url }, Cmd.none )

        HandleCustomVariantInput override text ->
            let
                newOverride : Override
                newOverride =
                    { override
                        | customVariantText = text
                    }

                newOverrides : List Override
                newOverrides =
                    replace override newOverride model.overrides

                newModel : Model
                newModel =
                    { model | overrides = newOverrides }
            in
            ( newModel, sendToLocalStorage <| encodeModel newModel )

        HandleVariantSelectionInput override selection ->
            let
                newOverride : Override
                newOverride =
                    { override | variantSelection = variantSelectionFromString selection, isSelected = True }

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

        OpenGithub ->
            ( model, createTab "https://github.com/tristanpendergrass/som" )

        HandleAddOverrideFeatureInput feature ->
            ( { model | feature = feature }, Cmd.none )

        HandleAddOverrideSubmit ->
            let
                newOverride : Override
                newOverride =
                    Override model.nonce model.feature OnVariant "" True

                newOverrides : List Override
                newOverrides =
                    newOverride :: model.overrides

                newModel : Model
                newModel =
                    { model | nonce = model.nonce + 1, overrides = newOverrides, feature = "" }
            in
            ( newModel, sendToLocalStorage <| encodeModel newModel )

        FocusResult result ->
            -- handle success or failure here
            case result of
                Err (Browser.Dom.NotFound _) ->
                    ( model, Cmd.none )

                Ok () ->
                    ( model, Cmd.none )

        SetFeatureEdit maybeOverride ->
            case ( maybeOverride, model.featureEditState ) of
                ( Nothing, NotEditing ) ->
                    ( model, Cmd.none )

                ( Nothing, Editing previousOverride draft _ ) ->
                    let
                        newModel =
                            { model
                                | featureEditState = NotEditing
                                , overrides = replace previousOverride { previousOverride | feature = getDraftValue draft } model.overrides
                            }
                    in
                    ( newModel, sendToLocalStorage <| encodeModel newModel )

                ( Just override, NotEditing ) ->
                    ( { model | featureEditState = Editing override (DraftValue override.feature) (OriginalValue override.feature) }
                    , Browser.Dom.focus featureInputId |> Task.attempt FocusResult
                    )

                ( Just override, Editing previousOverride _ originalValue ) ->
                    let
                        newModel =
                            { model
                                | featureEditState = Editing override (DraftValue override.feature) (OriginalValue override.feature)
                                , overrides = replace previousOverride { previousOverride | feature = getOriginalValue originalValue } model.overrides
                            }
                    in
                    ( newModel, sendToLocalStorage <| encodeModel newModel )

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

        ToggleSelectOverride toggledOverride isChecked ->
            let
                newOverrides =
                    model.overrides
                        |> List.map
                            (\override ->
                                if override == toggledOverride then
                                    { override | isSelected = isChecked }

                                else
                                    override
                            )

                newModel : Model
                newModel =
                    { model | overrides = newOverrides }
            in
            ( newModel, sendToLocalStorage <| encodeModel newModel )



-- SUBSCRIPTIONS


port browserUrl : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    browserUrl (\url -> SetBrowserUrl url)



-- VIEW


featureInputId : String
featureInputId =
    "feature-input"


underlineInput : String
underlineInput =
    "ml-0 mt-0 block w-full px-0.5 border-0 border-b-2 border-gray-200 focus:outline-none focus:ring-0 focus:border-gray-900"


tooltip : String
tooltip =
    "tooltip relative"


tooltipText : String
tooltipText =
    "tooltip-text absolute whitespace-nowrap z-50 bg-gray-900 text-gray-100 text-sm py-1 px-2 rounded-sm transition duration-200 delay-300 text-sm"


iconButton : String
iconButton =
    "icon-button p-1 rounded group focus:outline-none hover:bg-gray-100"


primaryButton : String
primaryButton =
    "primary-button bg-blue-500 text-gray-100 p-2 rounded-lg font-bold antialiased hover:bg-blue-400 cursor-pointer"


primaryButtonDisabled : String
primaryButtonDisabled =
    "cursor-not-allowed opacity-50 hover:bg-blue-500"


renderAddOverride : Model -> Html Msg
renderAddOverride model =
    div [ class "flex items-center space-x-1 my-1" ]
        [ input [ type_ "checkbox", class "invisible" ] []
        , button [ class iconButton, onClick HandleAddOverrideSubmit, disabled (model.feature == "") ]
            [ FeatherIcons.plus
                |> FeatherIcons.withSize 12
                |> FeatherIcons.withClass "text-blue-500"
                |> FeatherIcons.toHtml []
            ]
        , form [ onSubmit HandleAddOverrideSubmit, class "flex-grow" ]
            [ input [ class underlineInput, value model.feature, onInput HandleAddOverrideFeatureInput, placeholder "New Feature Name" ] []
            ]
        ]


renderOverride : FeatureEditState -> Override -> Html Msg
renderOverride featureEditState override =
    let
        fadeIfInactive = class <| if override.isSelected then "" else "opacity-50"

        featureText =
            div [ class tooltip ]
                [ div [ class "truncate", fadeIfInactive] [ text override.feature ]
                , div [ class tooltipText ] [ text override.feature ]
                ]

        labelOrInput =
            case featureEditState of
                NotEditing ->
                    featureText

                Editing editingOverride draftValue _ ->
                    if editingOverride == override then
                        input
                            [ id featureInputId
                            , value (getDraftValue draftValue)
                            , onInput HandleFeatureDraftInput
                            , class "w-full"
                            , class underlineInput
                            , onBlur <| SetFeatureEdit Nothing
                            ]
                            []

                    else
                        featureText

        editButton =
            let
                hideEditButton =
                    case featureEditState of
                        NotEditing ->
                            False

                        Editing editingOverride _ _ ->
                            editingOverride == override
            in
            button
                [ class iconButton
                , classList [ ( "invisible", hideEditButton ) ]
                , onClick (SetFeatureEdit (Just override))
                ]
                [ FeatherIcons.edit2
                    |> FeatherIcons.withSize 12
                    |> FeatherIcons.withClass "text-blue-500 group-hover:text-blue-800"
                    |> FeatherIcons.toHtml []
                ]

        customVariantInput =
            let
                hideInput : Bool
                hideInput =
                    override.variantSelection /= CustomVariant
            in
            input
                [ onInput (HandleCustomVariantInput override)
                , value override.customVariantText
                , classList [ ( "hidden", hideInput ) ]
                , class underlineInput
                , style "width" "100px"
                ]
                []

        selectionCheckbox =
            input [ type_ "checkbox", onCheck <| ToggleSelectOverride override, checked <| override.isSelected ] []

        -- hard coding this value since setting both divs to flex-grow:1 wasn't working for some reason
        halfWidth =
            177

        titleColor =
            case override.variantSelection of
                OffVariant ->
                    "bg-red-100"

                OnVariant ->
                    "bg-green-100"

                CustomVariant ->
                    "bg-yellow-100"
    in
    div [ class "flex items-center space-x-1" ]
        [ selectionCheckbox
        , editButton
        , div [ class "flex-grow flex justify-between" ]
            [ div [ classList [ ( titleColor, override.isSelected ) ], style "width" (String.fromInt halfWidth ++ "px") ] [ labelOrInput ]
            , div [fadeIfInactive] [ text ":" ]
            , div
                [ class "flex justify-end space-x-1"
                , style "width" (String.fromInt halfWidth ++ "px")
                , fadeIfInactive
                ]
                [ customVariantInput
                , select
                    [ onInput (HandleVariantSelectionInput override) ]
                    [ option [ value "OffVariant", selected (override.variantSelection == OffVariant) ] [ text "OFF" ]
                    , option [ value "OnVariant", selected (override.variantSelection == OnVariant) ] [ text "ON" ]
                    , option [ value "CustomVariant", selected (override.variantSelection == CustomVariant) ] [ text "Custom" ]
                    ]
                ]
            ]
        , div [ class tooltip ]
            [ button [ class iconButton, onClick (Archive override) ]
                [ FeatherIcons.archive
                    |> FeatherIcons.withSize 12
                    |> FeatherIcons.withClass "text-red-500"
                    |> FeatherIcons.toHtml []
                ]
            , div [ class tooltipText, class "-ml-12" ] [ text "Archive" ]
            ]
        ]


renderFeatureFilter : Model -> Html Msg
renderFeatureFilter model =
    -- Hiding this for the sake of visual clarity for now; can unhide it if we decide we need the feature later
    input
        [ class underlineInput
        , class "hidden"
        , onInput HandleFeatureFilterInput
        , value model.featureFilter
        , placeholder "Filter by feature name"
        ]
        []


renderTabs : Model -> Html Msg
renderTabs model =
    let
        tabClasses =
            "inline-block text-lg uppercase cursor-pointer"

        activeTabClasses =
            "underline font-bold text-black"

        inactiveTabClasses =
            "text-gray-500 hover:underline"
    in
    div [ class "flex justify-center mx-4 space-x-4" ]
        [ h2
            [ class tabClasses
            , classList [ ( activeTabClasses, model.activeTab == MainTab ), ( inactiveTabClasses, model.activeTab /= MainTab ) ]
            , onClick (SetActiveTab MainTab)
            ]
            [ text "Main" ]
        , h2
            [ class tabClasses
            , classList [ ( activeTabClasses, model.activeTab == ArchiveTab ), ( inactiveTabClasses, model.activeTab /= ArchiveTab ) ]
            , onClick (SetActiveTab ArchiveTab)
            ]
            [ text "Archive" ]
        ]


renderArchivedOverride : Override -> Html Msg
renderArchivedOverride override =
    div [ class "flex items-center" ]
        [ div [ class tooltip ]
            [ button [ class iconButton, onClick (Unarchive override) ]
                [ FeatherIcons.rotateCcw
                    |> FeatherIcons.withSize 12
                    |> FeatherIcons.withClass "text-blue-500"
                    |> FeatherIcons.toHtml []
                ]
            , div [ class tooltipText ] [ text "Unarchive" ]
            ]
        , div [ class "flex-grow truncate" ] [ text override.feature ]
        , div [ class tooltip ]
            [ button [ class iconButton, onClick (Delete override) ]
                [ FeatherIcons.trash2
                    |> FeatherIcons.withSize 12
                    |> FeatherIcons.withClass "text-red-500"
                    |> FeatherIcons.toHtml []
                ]
            , div [ class tooltipText, class "-ml-32" ] [ text "Permanently Delete" ]
            ]
        ]


renderHeader : Html Msg
renderHeader =
    h1 [ class "text-xl text-center border-b border-black" ] [ text "Stormcrow Override Manager" ]


renderApplyOverridesButton : Model -> Html Msg
renderApplyOverridesButton model =
    let
        isDisabled =
            not <| List.any .isSelected model.overrides
    in
    button
        [ class primaryButton
        , classList [ ( primaryButtonDisabled, isDisabled ) ]
        , onClick ApplyOverrides
        , disabled isDisabled
        ]
        [ text "Apply Overrides" ]


renderFooter : Html Msg
renderFooter =
    div [ class "flex justify-end items-center space-x-1" ]
        [ div [ class "text-gray-500" ] [ text "v0.1" ]
        , button [ class iconButton, class "flex items-center space-x-1 py-0", onClick OpenGithub ]
            [ div [] [ text "GitHub" ]
            , FeatherIcons.externalLink
                |> FeatherIcons.withSize 12
                |> FeatherIcons.toHtml []
            ]
        ]


view : Model -> Html Msg
view model =
    let
        bodyClasses =
            "flex flex-col h-full w-screen p-2"
    in
    case model.activeTab of
        MainTab ->
            div [ class bodyClasses ]
                [ renderHeader
                , renderTabs model
                , div [ class "flex justify-center my-2" ]
                    [ renderApplyOverridesButton model ]
                , renderFeatureFilter model
                , div
                    [ class "flex-grow overflow-y-auto space-y-0.5" ]
                    (renderAddOverride model
                        :: (if List.isEmpty model.overrides then
                                [ div [ class "w-full mt-32 text-center" ] [ text "No overrides exist." ] ]

                            else
                                model.overrides
                                    |> List.filter (.feature >> matchString model.featureFilter)
                                    |> List.map (renderOverride model.featureEditState)
                           )
                    )
                , renderFooter
                ]

        ArchiveTab ->
            div [ class bodyClasses ]
                [ renderHeader
                , renderTabs model
                , if List.isEmpty model.archivedOverrides then
                    div [ class "w-full mt-32 text-center" ] [ text "Archive is empty." ]

                  else
                    div [ class "overflow-y-scroll h-full" ]
                        (List.map
                            renderArchivedOverride
                            model.archivedOverrides
                        )
                , renderFooter
                ]