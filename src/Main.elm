port module Main exposing (main)

-- import Parser exposing (Parser)

import Browser
import Browser.Dom
import FeatherIcons
import Html exposing (Html, a, button, div, form, h1, h2, input, label, li, option, select, span, text, textarea, ul)
import Html.Attributes exposing (attribute, checked, class, classList, disabled, for, href, id, name, placeholder, selected, style, tabindex, type_, value)
import Html.Events exposing (onBlur, onCheck, onClick, onInput, onSubmit)
import Html.Keyed
import Json.Decode as D
import Json.Encode as E
import Json.Encode.Extra
import List
import Maybe.Extra
import ParseUserInput
import QueryParams exposing (QueryParam)
import Task
import Time
import Url exposing (Url)


main : Program ( String, D.Value ) Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


{-| Used to filter features by the input of the user
-}
matchString : String -> String -> Bool
matchString left right =
    String.contains (String.toLower left) (String.toLower right)


variantSelectionToString : VariantSelection -> String
variantSelectionToString value =
    case value of
        OnVariant ->
            "OnVariant"

        OffVariant ->
            "OffVariant"

        V1Variant ->
            "V1Variant"

        V2Variant ->
            "V2Variant"

        CustomVariant ->
            "CustomVariant"


variantSelectionFromString : String -> VariantSelection
variantSelectionFromString value =
    case value of
        "OnVariant" ->
            OnVariant

        "OffVariant" ->
            OffVariant

        "V1Variant" ->
            V1Variant

        "V2Variant" ->
            V2Variant

        "CustomVariant" ->
            CustomVariant

        _ ->
            OffVariant


legacyToNew : LegacyOverride -> Override
legacyToNew { id, feature, variantSelection, customVariantText } =
    { id = id, feature = feature, variantSelection = variantSelection, customVariantText = customVariantText, isAnimating = False }



-- MODEL


type alias Id =
    Int


type alias Feature =
    String


type VariantSelection
    = OnVariant
    | OffVariant
    | V1Variant
    | V2Variant
    | CustomVariant


type alias Override =
    { id : Id
    , feature : Feature
    , variantSelection : VariantSelection
    , customVariantText : String
    , isAnimating : Bool
    }


type alias LegacyOverride =
    { id : Id
    , feature : Feature
    , variantSelection : VariantSelection
    , customVariantText : String
    , isSelected : Bool
    }


type ActiveTab
    = MainTab
    | ArchiveTab
    | SettingsTab


type alias Model =
    { nonce : Int
    , browserUrl : Maybe Url
    , activeOverrides : List Override
    , inactiveOverrides : List Override
    , archivedOverrides : List Override
    , feature : Maybe String -- This is a Maybe because we only show the input to type a feature after user clicks a button
    , featureFilter : String
    , activeTab : ActiveTab
    , overrideToken : String
    , tokenCreatedAt : Maybe Time.Posix
    , showExportNotification : Bool
    , extensionDataToImport : String
    , importHasErr : Bool
    , theme : String
    }



-- Decoders


nonceDecoder : D.Decoder Int
nonceDecoder =
    D.oneOf
        [ D.field "nonce" D.int
        , D.succeed 100
        ]


overrideDecoder : D.Decoder Override
overrideDecoder =
    D.map5 Override
        (D.field "id" D.int)
        (D.field "feature" D.string)
        (D.field "variantSelection" (D.map variantSelectionFromString D.string))
        (D.field "customVariantText" D.string)
        (D.succeed False)


activeOverridesDecoder : D.Decoder (List Override)
activeOverridesDecoder =
    D.field "activeOverrides" (D.list overrideDecoder)


inactiveOverridesDecoder : D.Decoder (List Override)
inactiveOverridesDecoder =
    D.field "inactiveOverrides" (D.list overrideDecoder)


overridesDecoder : D.Decoder ( List Override, List Override )
overridesDecoder =
    D.map2 Tuple.pair activeOverridesDecoder inactiveOverridesDecoder


overrideTokenDecoder : D.Decoder String
overrideTokenDecoder =
    D.field "overrideToken" D.string


tokenCreatedAtDecoder : D.Decoder (Maybe Time.Posix)
tokenCreatedAtDecoder =
    D.field "tokenCreatedAt" (D.nullable D.int)
        |> D.map (Maybe.map Time.millisToPosix)


themeDecoder : D.Decoder String
themeDecoder =
    D.field "theme" D.string


init : ( String, D.Value ) -> ( Model, Cmd Msg )
init ( initialBrowserUrl, localStorageData ) =
    let
        nonce : Int
        nonce =
            localStorageData
                |> D.decodeValue (D.field "nonce" D.int)
                |> Result.withDefault 100

        activeOverrides =
            localStorageData
                |> D.decodeValue activeOverridesDecoder
                |> Result.withDefault []

        inactiveOverrides =
            localStorageData
                |> D.decodeValue inactiveOverridesDecoder
                |> Result.withDefault []

        archivedOverrides : List Override
        archivedOverrides =
            localStorageData
                |> D.decodeValue (D.field "archivedOverrides" (D.list overrideDecoder))
                |> Result.withDefault []

        overrideToken : String
        overrideToken =
            localStorageData
                |> D.decodeValue overrideTokenDecoder
                |> Result.withDefault ""

        tokenCreatedAt : Maybe Time.Posix
        tokenCreatedAt =
            localStorageData
                |> D.decodeValue tokenCreatedAtDecoder
                |> Result.withDefault Nothing

        theme : String
        theme =
            localStorageData
                |> D.decodeValue themeDecoder
                |> Result.withDefault defaultTheme

        model : Model
        model =
            { nonce = nonce
            , browserUrl = Url.fromString initialBrowserUrl
            , activeOverrides = activeOverrides
            , inactiveOverrides = inactiveOverrides
            , archivedOverrides = archivedOverrides
            , feature = Nothing
            , featureFilter = ""
            , activeTab = MainTab
            , overrideToken = overrideToken
            , tokenCreatedAt = tokenCreatedAt
            , showExportNotification = False
            , extensionDataToImport = ""
            , importHasErr = False
            , theme = theme
            }
    in
    ( model
    , Task.perform CheckIfTokenExpired Time.now
    )


extensionDataDecoder : Model -> D.Decoder Model
extensionDataDecoder oldModel =
    let
        createNewModel : Int -> List Override -> List Override -> String -> Maybe Time.Posix -> String -> Model
        createNewModel nonce activeOverrides inactiveOverrides overrideToken tokenCreatedAt theme =
            { nonce = nonce
            , browserUrl = oldModel.browserUrl
            , activeOverrides = activeOverrides
            , inactiveOverrides = inactiveOverrides
            , archivedOverrides = []
            , feature = Nothing
            , featureFilter = ""
            , activeTab = oldModel.activeTab
            , overrideToken = overrideToken
            , tokenCreatedAt = tokenCreatedAt
            , showExportNotification = oldModel.showExportNotification
            , extensionDataToImport = oldModel.extensionDataToImport
            , importHasErr = False
            , theme = theme
            }
    in
    D.map6 createNewModel nonceDecoder activeOverridesDecoder inactiveOverridesDecoder overrideTokenDecoder tokenCreatedAtDecoder themeDecoder



-- UPDATE


port sendUrl : String -> Cmd msg


port sendToLocalStorage : E.Value -> Cmd msg


port createTab : String -> Cmd msg


port writeToClipboard : String -> Cmd msg


encodeModel : Model -> E.Value
encodeModel model =
    let
        overrideToJson : Override -> E.Value
        overrideToJson { id, feature, variantSelection, customVariantText } =
            E.object
                [ ( "id", E.int id )
                , ( "feature", E.string feature )
                , ( "variantSelection", E.string (variantSelectionToString variantSelection) )
                , ( "customVariantText", E.string customVariantText )
                ]
    in
    E.object
        [ ( "nonce", E.int model.nonce )
        , ( "activeOverrides", E.list overrideToJson model.activeOverrides )
        , ( "inactiveOverrides", E.list overrideToJson model.inactiveOverrides )
        , ( "archivedOverrides", E.list overrideToJson model.archivedOverrides )
        , ( "overrideToken", E.string model.overrideToken )
        , ( "tokenCreatedAt", Json.Encode.Extra.maybe (Time.posixToMillis >> E.int) model.tokenCreatedAt )
        , ( "theme", E.string model.theme )
        ]


applyOverrides : List Override -> List QueryParam -> List QueryParam
applyOverrides overrides oldQueryParams =
    overrides
        |> List.foldl
            (\override ->
                \accumulator ->
                    case override.variantSelection of
                        OnVariant ->
                            QueryParams.applyOverride override.feature "ON" accumulator

                        OffVariant ->
                            QueryParams.applyOverride override.feature "OFF" accumulator

                        V1Variant ->
                            QueryParams.applyOverride override.feature "V1" accumulator

                        V2Variant ->
                            QueryParams.applyOverride override.feature "V2" accumulator

                        CustomVariant ->
                            QueryParams.applyOverride override.feature override.customVariantText accumulator
            )
            oldQueryParams


makeQueryString : String -> List Override -> String
makeQueryString token overrides =
    let
        newQueryParams =
            []
                |> applyOverrides overrides
                |> QueryParams.setTtl (String.fromInt (60 * 60 * 24))
                |> QueryParams.setToken token
    in
    QueryParams.toString newQueryParams
        |> Maybe.map (\str -> "?" ++ str)
        |> Maybe.withDefault ""


makeUrl : String -> List Override -> Url -> Url
makeUrl token overrides oldUrl =
    let
        newQueryParams =
            QueryParams.fromUrl oldUrl
                |> List.filter (QueryParams.isStormcrowParam >> not)
                |> applyOverrides overrides
                |> QueryParams.setTtl (String.fromInt (60 * 60 * 24))
                |> QueryParams.setToken token

        newUrl : Url
        newUrl =
            { oldUrl | query = QueryParams.toString newQueryParams }
    in
    newUrl


type Msg
    = NoOp
    | SetBrowserUrl String
    | HandleFeatureFilterInput String
    | ApplyOverrides
    | SetActiveTab ActiveTab
    | FocusResult (Result Browser.Dom.Error ())
    | OpenGithub
    | OpenToken
    | HandleOverrideTokenInput String
    | SetTokenCreatedAt (Maybe Time.Posix)
    | CheckIfTokenExpired Time.Posix
    | CopyOverridesToClipboard
    | ExportExtensionData
    | ImportExtensionData
    | HandleExtensionDataInput String
    | SetTheme String
      -- Add Override
    | ToggleFeatureInput
    | HandleAddOverrideFeatureInput String
    | HandleAddOverrideFeatureBlur
    | HandleAddOverrideSubmit { keepOpen : Bool }
      -- Edit Override
    | HandleVariantSelectionInput Override String
    | HandleCustomVariantInput Override String
    | HandleFeatureInput Override String
    | ToggleSelectOverride Override Bool
    | DeactivateAllOverrides
    | Archive Override
    | Unarchive Override
    | Delete Override


replaceInTwoLists : a -> a -> ( List a, List a ) -> ( List a, List a )
replaceInTwoLists el newEl ( left, right ) =
    let
        replaceEl x =
            if x == el then
                newEl

            else
                x
    in
    if List.member el left then
        ( List.map replaceEl left, right )

    else if List.member el right then
        ( left, List.map replaceEl right )

    else
        ( left, right )


removeFromTwoLists : a -> ( List a, List a ) -> ( List a, List a )
removeFromTwoLists el ( left, right ) =
    if List.member el left then
        ( List.filter ((/=) el) left, right )

    else if List.member el right then
        ( left, List.filter ((/=) el) right )

    else
        ( left, right )


replaceOverride : Override -> Override -> Model -> Model
replaceOverride override newOverride model =
    let
        ( newActiveOverrides, newInactiveOverrides ) =
            replaceInTwoLists override newOverride ( model.activeOverrides, model.inactiveOverrides )
    in
    { model | activeOverrides = newActiveOverrides, inactiveOverrides = newInactiveOverrides }


removeOverride : Override -> Model -> Model
removeOverride override model =
    let
        ( newActiveOverrides, newInactiveOverrides ) =
            removeFromTwoLists override ( model.activeOverrides, model.inactiveOverrides )
    in
    { model | activeOverrides = newActiveOverrides, inactiveOverrides = newInactiveOverrides }


addActiveOverride : Override -> Model -> Model
addActiveOverride override model =
    { model | activeOverrides = override :: model.activeOverrides }


removeOverrideWithFeature : String -> Model -> Model
removeOverrideWithFeature removedFeature model =
    let
        overrideDoesNotHaveFeature : Override -> Bool
        overrideDoesNotHaveFeature { feature } =
            feature /= removedFeature
    in
    { model
        | activeOverrides = List.filter overrideDoesNotHaveFeature model.activeOverrides
        , inactiveOverrides = List.filter overrideDoesNotHaveFeature model.inactiveOverrides
    }


incrementNonce : Model -> Model
incrementNonce model =
    { model | nonce = model.nonce + 1 }


createOverride : Int -> String -> String -> Override
createOverride id featureName value =
    if String.toUpper value == "ON" then
        Override id featureName OnVariant "" True

    else if String.toUpper value == "OFF" then
        Override id featureName OffVariant "" True

    else
        Override id featureName CustomVariant value True


addSubmittedOverride : ( String, String ) -> Model -> Model
addSubmittedOverride ( featureName, value ) model =
    model
        |> removeOverrideWithFeature featureName
        |> addActiveOverride (createOverride model.nonce featureName value)
        |> incrementNonce


addSubmittedOverrides : List ( String, String ) -> Model -> Model
addSubmittedOverrides submittedOverrides model =
    List.foldl addSubmittedOverride model submittedOverrides


addInactiveOverride : Override -> Model -> Model
addInactiveOverride override model =
    { model | inactiveOverrides = { override | isAnimating = True } :: model.inactiveOverrides }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        noOp =
            ( model, Cmd.none )
    in
    case msg of
        NoOp ->
            noOp

        SetBrowserUrl url ->
            ( { model | browserUrl = Url.fromString url }, Cmd.none )

        HandleCustomVariantInput override text ->
            let
                newOverride : Override
                newOverride =
                    { override
                        | customVariantText = text
                    }

                newModel : Model
                newModel =
                    model
                        |> replaceOverride override newOverride
            in
            ( newModel, sendToLocalStorage <| encodeModel newModel )

        HandleVariantSelectionInput override selection ->
            let
                ( newOverride, commands ) =
                    case selection of
                        "ON" ->
                            ( { override | variantSelection = OnVariant }, Cmd.none )

                        "OFF" ->
                            ( { override | variantSelection = OffVariant }, Cmd.none )

                        "V1" ->
                            ( { override | variantSelection = V1Variant }, Cmd.none )

                        "V2" ->
                            ( { override | variantSelection = V2Variant }, Cmd.none )

                        _ ->
                            ( { override | variantSelection = CustomVariant }
                              -- Focus the custom input so user notices it
                            , Task.attempt (\_ -> NoOp) (Browser.Dom.focus (domIdForCustomVariantInput override))
                            )

                newModel : Model
                newModel =
                    model
                        |> replaceOverride override newOverride
            in
            ( newModel
            , Cmd.batch
                [ sendToLocalStorage <| encodeModel newModel
                , commands
                ]
            )

        ApplyOverrides ->
            case model.browserUrl of
                Nothing ->
                    ( model, Cmd.none )

                Just oldUrl ->
                    let
                        newUrl : Url
                        newUrl =
                            makeUrl model.overrideToken model.activeOverrides oldUrl
                    in
                    ( model, sendUrl <| Url.toString newUrl )

        SetActiveTab activeTab ->
            ( { model
                | activeTab = activeTab

                -- Annoys me that if you open the feature input then change to settings then change back it's still open
                , feature =
                    if model.feature == Just "" then
                        Nothing

                    else
                        model.feature
                , showExportNotification = False
              }
            , Cmd.none
            )

        OpenGithub ->
            ( model, createTab "https://github.com/tristanpendergrass/som" )

        OpenToken ->
            ( model, createTab "https://www.dropbox.com/admin/stormcrow#/override" )

        HandleOverrideTokenInput newString ->
            let
                -- The token link
                stringToDrop =
                    "&override_token="

                tokenString =
                    if String.startsWith stringToDrop newString then
                        String.dropLeft (String.length stringToDrop) newString

                    else
                        newString

                newModel : Model
                newModel =
                    { model | overrideToken = tokenString }
            in
            ( newModel, Cmd.batch [ sendToLocalStorage <| encodeModel newModel, Task.perform (Just >> SetTokenCreatedAt) Time.now ] )

        SetTokenCreatedAt maybeTime ->
            let
                newModel : Model
                newModel =
                    { model | tokenCreatedAt = maybeTime }
            in
            ( newModel, sendToLocalStorage <| encodeModel newModel )

        CheckIfTokenExpired now ->
            let
                -- We reset the token after 24 hours because that's when a token will stop working.
                timeToReset =
                    --1000 * 60 * 60 * 24 -- 24 hours in millis
                    1000 * 60 * 10

                -- 10 min in millis
                shouldResetToken =
                    model.tokenCreatedAt
                        |> Maybe.map
                            (\createdAt ->
                                Time.posixToMillis now - Time.posixToMillis createdAt > timeToReset
                            )
                        |> Maybe.withDefault False

                newModel : Model
                newModel =
                    if shouldResetToken then
                        { model | tokenCreatedAt = Nothing, overrideToken = "" }

                    else
                        model
            in
            ( newModel, sendToLocalStorage <| encodeModel newModel )

        CopyOverridesToClipboard ->
            ( model, writeToClipboard <| makeQueryString model.overrideToken model.activeOverrides )

        ExportExtensionData ->
            ( { model | showExportNotification = True }
              -- Intentionally not writing the showExportNotification = True to the encoded model since it shouldn't be
            , writeToClipboard <| E.encode 0 (encodeModel model)
            )

        ImportExtensionData ->
            let
                newModel =
                    model.extensionDataToImport
                        |> D.decodeString (extensionDataDecoder model)
                        -- If successful change tab to Main
                        |> Result.map (\updatedModel -> { updatedModel | activeTab = MainTab, extensionDataToImport = "" })
                        -- If failure make textarea an error
                        |> Result.withDefault { model | importHasErr = True }
            in
            ( newModel, sendToLocalStorage <| encodeModel newModel )

        HandleExtensionDataInput newValue ->
            let
                newModel =
                    { model | extensionDataToImport = newValue, importHasErr = False }
            in
            ( newModel, sendToLocalStorage <| encodeModel newModel )

        SetTheme theme ->
            let
                newModel =
                    { model | theme = theme }
            in
            ( newModel, sendToLocalStorage <| encodeModel newModel )

        ToggleFeatureInput ->
            let
                newFeatureValue =
                    case model.feature of
                        Nothing ->
                            Just ""

                        Just _ ->
                            Nothing
            in
            ( { model | feature = newFeatureValue }
            , Task.attempt (\_ -> NoOp) (Browser.Dom.focus featureInputId)
            )

        HandleAddOverrideFeatureInput feature ->
            ( { model | feature = Just feature }, Cmd.none )

        HandleAddOverrideFeatureBlur ->
            if model.feature == Just "" then
                ( { model | feature = Nothing }, Cmd.none )

            else
                noOp

        HandleAddOverrideSubmit { keepOpen } ->
            case model.feature of
                Nothing ->
                    noOp

                Just "" ->
                    noOp

                Just featureText ->
                    let
                        submittedOverrides =
                            ParseUserInput.parseUserInput featureText

                        newFeature =
                            if keepOpen then
                                Just ""

                            else
                                Nothing

                        additionalCmds =
                            if keepOpen then
                                [ Task.attempt (\_ -> NoOp) (Browser.Dom.focus featureInputId) ]

                            else
                                []

                        newModel : Model
                        newModel =
                            model
                                |> addSubmittedOverrides submittedOverrides
                                |> (\oldModel -> { oldModel | feature = newFeature })
                    in
                    ( newModel
                    , Cmd.batch <|
                        List.concat
                            [ [ sendToLocalStorage <| encodeModel newModel ]
                            , additionalCmds
                            ]
                    )

        FocusResult result ->
            -- handle success or failure here
            case result of
                Err (Browser.Dom.NotFound _) ->
                    ( model, Cmd.none )

                Ok () ->
                    ( model, Cmd.none )

        HandleFeatureInput override value ->
            let
                newModel =
                    model
                        |> replaceOverride override { override | feature = value }
            in
            ( newModel, sendToLocalStorage <| encodeModel newModel )

        HandleFeatureFilterInput value ->
            ( { model | featureFilter = value }, Cmd.none )

        Archive override ->
            let
                newModel : Model
                newModel =
                    model
                        |> removeOverride override
                        |> (\oldModel -> { oldModel | archivedOverrides = override :: model.archivedOverrides })
            in
            ( newModel, sendToLocalStorage <| encodeModel newModel )

        Unarchive override ->
            let
                newModel : Model
                newModel =
                    { model
                        | activeOverrides = override :: model.activeOverrides
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

        ToggleSelectOverride override isChecked ->
            let
                addToList =
                    if isChecked then
                        addActiveOverride

                    else
                        addInactiveOverride

                newModel : Model
                newModel =
                    model
                        |> removeOverride override
                        |> addToList { override | isAnimating = True }
            in
            ( newModel, sendToLocalStorage <| encodeModel newModel )

        DeactivateAllOverrides ->
            let
                newModel : Model
                newModel =
                    { model
                        | activeOverrides = []
                        , inactiveOverrides = List.concat [ model.activeOverrides, model.inactiveOverrides ]
                    }
            in
            ( newModel, sendToLocalStorage <| encodeModel newModel )



-- SUBSCRIPTIONS


port browserUrl : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    browserUrl (\url -> SetBrowserUrl url)



-- VIEW


underlineInput : String
underlineInput =
    "ml-0 mt-0 block w-full border-0 border-b-2 border-gray-200 focus:outline-none focus:ring-0 focus:border-gray-900"


featureInputId : String
featureInputId =
    "feature-input"


type ToggleButtonMode
    = On
    | Off


overrideAddToggleButton : { isVisible : Bool, isAdd : Bool } -> Html Msg
overrideAddToggleButton { isVisible, isAdd } =
    label
        [ class "swap swap-rotate"
        , classList [ ( "invisible", not isVisible ), ( "swap-active", isAdd ) ]
        , style "width" "1.25rem"
        , style "min-width" "1.25rem"
        , style "height" "1.25rem"
        , style "min-height" "1.25rem"
        , onClick ToggleFeatureInput
        ]
        [ FeatherIcons.plus
            |> FeatherIcons.withClass "swap-on"
            |> FeatherIcons.withSize 12
            |> FeatherIcons.toHtml []
        , FeatherIcons.x
            |> FeatherIcons.withClass "swap-off"
            |> FeatherIcons.withSize 12
            |> FeatherIcons.toHtml []
        ]


infoBoxHeight : String
infoBoxHeight =
    -- This needs to be kept as a constant so the box can be animated to this height
    "h-[5.5rem]"


renderAddOverrideButtons : { isDisabled : Bool, isVisible : Bool } -> Html Msg
renderAddOverrideButtons { isDisabled, isVisible } =
    div [ class "flex space-x-1", classList [ ( "invisible", not isVisible ) ] ]
        [ div [ class "tooltip tooltip-left", attribute "data-tip" "Add Override" ]
            [ button
                [ class "btn btn-sm btn-square btn-primary"
                , disabled isDisabled
                , onClick <| HandleAddOverrideSubmit { keepOpen = False }
                ]
                [ FeatherIcons.check
                    |> FeatherIcons.withSize 12
                    |> FeatherIcons.toHtml []
                ]
            ]
        , div [ class "tooltip tooltip-left", attribute "data-tip" "Add and keep open" ]
            [ button
                [ class "btn btn-sm gap-2 btn-secondary"
                , disabled isDisabled
                , onClick <| HandleAddOverrideSubmit { keepOpen = True }
                ]
                [ FeatherIcons.check
                    |> FeatherIcons.withSize 12
                    |> FeatherIcons.toHtml []
                , FeatherIcons.refreshCcw
                    |> FeatherIcons.withSize 12
                    |> FeatherIcons.toHtml []
                ]
            ]
        ]


renderAddOverride : { feature : Maybe String } -> Html Msg
renderAddOverride { feature } =
    form [ onSubmit <| HandleAddOverrideSubmit { keepOpen = False }, class "flex flex-col space-y-1" ]
        [ div [ class "flex w-full h-9 items-center space-x-2" ]
            [ overrideAddToggleButton
                { isVisible = True
                , isAdd = Maybe.Extra.isNothing feature
                }
            , div [ class "flex-grow" ]
                [ case feature of
                    Just featureText ->
                        input
                            [ type_ "text"
                            , id featureInputId
                            , placeholder "feature_name"
                            , value featureText
                            , class "input input-xs input-bordered w-full input-primary"
                            , onInput HandleAddOverrideFeatureInput

                            -- , onBlur HandleAddOverrideFeatureBlur -- This causes a bug where when you click the close button the input remains open.
                            ]
                            []

                    Nothing ->
                        div
                            [ class "font-semibold cursor-pointer hover:text-primary"
                            , onClick ToggleFeatureInput
                            ]
                            [ text "Add override" ]
                ]
            , case feature of
                Just featureText ->
                    renderAddOverrideButtons { isDisabled = featureText == "", isVisible = True }

                Nothing ->
                    div [] []
            ]
        , div
            [ class "w-full transition-height overflow-hidden flex space-x-2 items-center"
            , classList [ ( infoBoxHeight, Maybe.Extra.isJust feature ), ( "h-0", Maybe.Extra.isNothing feature ) ]
            ]
            [ overrideAddToggleButton { isVisible = False, isAdd = True }
            , div [ class "flex-grow" ] [ renderInfoBox ]
            , renderAddOverrideButtons { isVisible = False, isDisabled = False }
            ]
        ]


renderInfoBox : Html Msg
renderInfoBox =
    div [ class "w-full bg-info text-info-content rounded flex items-center space-x-3 px-3", class infoBoxHeight ]
        [ FeatherIcons.info
            |> FeatherIcons.withSize 16
            |> FeatherIcons.withClass "inline-block"
            |> FeatherIcons.toHtml []
        , div [ class "flex flex-col space-y-1 flex-grow" ]
            [ div [ class "font-bold text-xs" ] [ text "Supported formats" ]
            , ul [ class "italic text-2xs " ]
                [ li [] [ text "my_feature" ]
                , li [] [ text "my_feature:V1" ]
                , li [] [ text "www.dropbox.com/home?stormcrow_override=my_feature:V1" ]
                ]
            ]
        ]


overrideCheckbox : { isChecked : Bool, handleCheck : Bool -> Msg } -> Html Msg
overrideCheckbox { isChecked, handleCheck } =
    input [ type_ "checkbox", checked isChecked, onCheck handleCheck, class "checkbox checkbox-sm" ] []


overrideDeleteButton : { handleDelete : Msg } -> Html Msg
overrideDeleteButton { handleDelete } =
    div [ class "tooltip tooltip-left", attribute "data-tip" "Delete" ]
        [ button [ class "btn btn-square btn-ghost btn-sm", onClick handleDelete ]
            [ FeatherIcons.trash2
                |> FeatherIcons.withSize 12
                |> FeatherIcons.toHtml []
            ]
        ]


domIdForCustomVariantInput : Override -> String
domIdForCustomVariantInput { id } =
    "custom-variant-input-" ++ String.fromInt id


renderActiveOverride : Override -> Html Msg
renderActiveOverride override =
    let
        contextualColors =
            case override.variantSelection of
                OffVariant ->
                    "bg-error/50 text-error-content"

                OnVariant ->
                    "bg-success/50 text-success-content"

                _ ->
                    "bg-warning/50 text-warning-content"

        baseColors =
            "bg-base-200 text-base-content"
    in
    div [ class "flex w-full h-9 items-center space-x-2", classList [ ( "animate-flash", override.isAnimating ) ] ]
        [ overrideCheckbox { isChecked = True, handleCheck = ToggleSelectOverride override }
        , div [ class "basis-3/4" ]
            [ input
                [ type_ "text"
                , value override.feature
                , class "input input-bordered input-xs w-full"
                , onInput (HandleFeatureInput override)
                ]
                []
            ]
        , div [ class "basis-1/4", classList [ ( "invisible", override.variantSelection /= CustomVariant ) ] ]
            [ input
                [ type_ "text"
                , id <| domIdForCustomVariantInput override
                , value override.customVariantText
                , class "input input-bordered input-xs w-full input-warning"
                , style "min-width" "20px"
                , onInput <| HandleCustomVariantInput override
                ]
                []
            ]
        , select
            [ class "select select-bordered select-xs"
            , class contextualColors
            , style "max-width" "4rem"
            , onInput <| HandleVariantSelectionInput override
            ]
            [ option [ selected (override.variantSelection == OnVariant), value "ON", class baseColors ] [ text "ON" ]
            , option [ selected (override.variantSelection == OffVariant), value "OFF", class baseColors ] [ text "OFF" ]
            , option [ selected (override.variantSelection == V1Variant), value "V1", class baseColors ] [ text "V1" ]
            , option [ selected (override.variantSelection == V2Variant), value "V2", class baseColors ] [ text "V2" ]
            , option [ selected (override.variantSelection == CustomVariant), value "CustomVariant", class baseColors ] [ text "Custom" ]
            ]
        , overrideDeleteButton { handleDelete = Archive override }
        ]


renderInactiveOverride : Override -> Html Msg
renderInactiveOverride override =
    div [ class "flex w-full h-9 items-center space-x-2", classList [ ( "animate-flash", override.isAnimating ) ] ]
        [ overrideCheckbox { isChecked = False, handleCheck = ToggleSelectOverride override }
        , div [ class "flex-grow pl-2" ] [ div [] [ text override.feature ] ]
        , overrideDeleteButton { handleDelete = Archive override }
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


renderHeader : Model -> Html Msg
renderHeader model =
    let
        { settingsIconClass, closeIconClass, handleClick } =
            case model.activeTab of
                SettingsTab ->
                    { settingsIconClass = "swap-on", closeIconClass = "swap-off", handleClick = SetActiveTab MainTab }

                _ ->
                    { settingsIconClass = "swap-off", closeIconClass = "swap-on", handleClick = SetActiveTab SettingsTab }
    in
    div [ class "w-full flex justify-between items-center bg-neutral text-neutral-content p-4" ]
        [ div [ class "text-xl text-left font-extrabold" ] [ text "Stormcrow Override Manager" ]
        , label [ class "btn btn-circle btn-ghost btn-sm swap swap-rotate", onClick handleClick ]
            [ FeatherIcons.settings
                |> FeatherIcons.withClass settingsIconClass
                |> FeatherIcons.withSize 16
                |> FeatherIcons.toHtml []
            , FeatherIcons.x
                |> FeatherIcons.withClass closeIconClass
                |> FeatherIcons.withSize 16
                |> FeatherIcons.toHtml []
            ]
        ]


renderActionBar : Model -> Html Msg
renderActionBar model =
    let
        isDisabled =
            List.isEmpty model.activeOverrides
    in
    div [ class "flex w-full justify-between items-center" ]
        [ div [ class "flex space-x-1" ]
            [ button
                [ class "btn btn-primary btn-sm gap-2"
                , onClick ApplyOverrides
                , disabled isDisabled
                ]
                [ FeatherIcons.zap
                    |> FeatherIcons.withClass "inline-block"
                    |> FeatherIcons.withSize 16
                    |> FeatherIcons.toHtml []
                , text "Override"
                ]
            , button
                [ class "btn btn-secondary btn-sm gap-2 text-secondary-content"
                , onClick CopyOverridesToClipboard
                , disabled isDisabled
                ]
                [ FeatherIcons.clipboard
                    |> FeatherIcons.withSize 16
                    |> FeatherIcons.toHtml []
                , text "Copy"
                ]
            ]
        ]


renderFooter : Html Msg
renderFooter =
    div [ class "flex justify-between items-center p-4 bg-neutral text-neutral-content" ]
        [ div [] [ span [] [ text "Feedback? Message " ], span [ class "text-primary" ] [ text "@tristanp" ] ]
        , div [ class "flex items-center space-x-2" ]
            [ div [ class "font-mono text-opacity-50" ] [ text "v3.1" ]
            , button [ class "flex items-center space-x-1 py-0", onClick OpenGithub ]
                [ div [] [ text "GitHub" ]
                , FeatherIcons.externalLink
                    |> FeatherIcons.withSize 12
                    |> FeatherIcons.toHtml []
                ]
            ]
        ]


renderMainTab : Model -> Html Msg
renderMainTab model =
    div [ class "h-[27rem]" ]
        [ div [ class "flex justify-left my-2" ]
            [ renderActionBar model ]
        , renderFeatureFilter model
        , div
            [ class "flex-grow" ]
            (if List.isEmpty model.activeOverrides && List.isEmpty model.inactiveOverrides then
                [ renderAddOverride { feature = model.feature }
                , div [ class "w-full mt-32 text-center" ] [ text "No overrides exist." ]
                ]

             else
                [ renderAddOverride { feature = model.feature }

                -- Active overrides
                , Html.Keyed.node "div"
                    [ class "flex flex-col w-full" ]
                    (model.activeOverrides
                        |> List.filter (.feature >> matchString model.featureFilter)
                        |> List.map
                            (\override ->
                                ( String.fromInt override.id, renderActiveOverride override )
                            )
                    )
                , div [ class "divider", classList [ ( "hidden", List.isEmpty model.inactiveOverrides ) ] ] [ text "Inactive" ]

                -- Inactive overrides
                , div [ class "flex flex-col w-full" ]
                    (model.inactiveOverrides
                        |> List.filter (.feature >> matchString model.featureFilter)
                        |> List.map renderInactiveOverride
                    )
                ]
            )
        ]



-- Theme Changer


themes : List String
themes =
    [ "light", "dark", "cupcake", "bumblebee", "emerald", "corporate", "synthwave", "retro", "cyberpunk", "valentine", "halloween", "garden", "forest", "aqua", "lofi", "pastel", "fantasy", "wireframe", "black", "luxury", "dracula", "cmyk", "autumn", "business", "acid", "lemonade", "night", "coffee", "winter" ]


defaultTheme : String
defaultTheme =
    "corporate"


renderThemeChange : { selectedTheme : String } -> Html Msg
renderThemeChange { selectedTheme } =
    div [ class "dropdown" ]
        [ div [ tabindex 0, class "btn btn-primary btn-sm gap-2 normal-case" ]
            [ FeatherIcons.edit3
                |> FeatherIcons.withSize 12
                |> FeatherIcons.toHtml []
            , text selectedTheme
            , FeatherIcons.arrowDown
                |> FeatherIcons.withSize 12
                |> FeatherIcons.toHtml []
            ]
        , div [ class "dropdown-content bg-base-200 text-base-content rounded-t-box rounded-b-box top-px max-h-96 h-[60vh] w-52 overflow-y-auto shadow-2xl mt-12" ]
            [ div [ class "grid grid-cols-1 gap-3 p-3", tabindex 0 ]
                (themes
                    |> List.map
                        (\theme ->
                            div
                                [ class "outline-base-content overflow-hidden rounded-lg outline outline-2 outline-offset-2"
                                , classList
                                    [ ( "outline-base-content", True )
                                    , ( "outline-accent", False )
                                    ]
                                , onClick (SetTheme theme)
                                ]
                                [ div
                                    [ attribute "data-theme" theme
                                    , class "bg-base-100 text-base-content w-full cursor-pointer font-sans"
                                    ]
                                    [ div [ class "grid grid-cols-5 grid-rows-3" ]
                                        [ div [ class "col-span-5 row-span-3 row-start-1 flex gap-1 py-3 px-4" ]
                                            [ div [ class "flex-grow text-sm font-bold" ] [ text theme ]
                                            , div [ class "flex flex-shrink-0 flex-wrap gap-1" ]
                                                [ div [ class "bg-primary w-2 rounded" ] []
                                                , div [ class "bg-secondary w-2 rounded" ] []
                                                , div [ class "bg-accent w-2 rounded" ] []
                                                , div [ class "bg-neutral w-2 rounded" ] []
                                                ]
                                            ]
                                        ]
                                    ]
                                ]
                        )
                )
            ]
        ]


renderSettingsTab : Model -> Html Msg
renderSettingsTab model =
    let
        optionContainer =
            "w-full bg-base-300 rounded shadow-xl p-3"

        optionTitle =
            "font-semibold text-lg"
    in
    div [ class "h-[27rem] w-full flex flex-col pt-3 overflow-hidden" ]
        [ div [ class "flex w-full justify-center items-center" ]
            [ span [ class "text-xl font-bold" ] [ text "Settings" ]
            ]
        , div [ class "flex-col w-100 items-start my-4 space-y-4 h-full" ]
            [ div [ class optionContainer, class "flex flex-col space-y-1" ]
                [ h2 [ class optionTitle ] [ text "Theme" ]
                , div [ class "flex items-center justify-between space-x-1" ]
                    [ renderThemeChange { selectedTheme = model.theme }
                    , button
                        [ class "btn btn-primary btn-sm"
                        , classList [ ( "invisible", model.theme == defaultTheme ) ]
                        , onClick <| SetTheme defaultTheme
                        , attribute "data-theme" defaultTheme
                        ]
                        [ text "Restore Default" ]
                    ]
                ]
            , div [ class optionContainer ]
                [ div [ class "w-full flex flex-col space-y-2 h-[9rem]" ]
                    [ h2 [ class optionTitle ] [ text "Extension Data" ]
                    , div [ class "flex items-start h-full" ]
                        [ div [ class "flex flex-col w-1/2 space-y-2 items-center" ]
                            [ div [ class "relative" ]
                                [ button [ class "btn btn-sm gap-2", onClick ExportExtensionData ]
                                    [ FeatherIcons.arrowUp
                                        |> FeatherIcons.withSize 16
                                        |> FeatherIcons.toHtml []
                                    , text "Export"
                                    ]
                                , div
                                    [ class "absolute bottom-[-2rem] w-[10.5rem] left-[50%] translate-x-[-50%]"
                                    , class "bg-success text-success-content gap-2 flex px-2 items-center h-7 rounded shadow overflow-hidden text-2xs"

                                    -- handle the fade in
                                    , classList [ ( "opacity-100", model.showExportNotification ), ( "opacity-0", not model.showExportNotification ) ]
                                    , class "transition-opacity duration-100"
                                    ]
                                    [ FeatherIcons.check
                                        |> FeatherIcons.withSize 16
                                        |> FeatherIcons.toHtml []
                                    , text "Data copied to clipboard"
                                    ]
                                ]
                            ]
                        , div [ class "divider divider-horizontal" ] []
                        , div [ class "flex flex-col w-1/2 space-y-2 items-center" ]
                            [ button [ class "btn btn-sm gap-2", disabled (String.isEmpty model.extensionDataToImport), onClick ImportExtensionData ]
                                [ FeatherIcons.arrowDown
                                    |> FeatherIcons.withSize 16
                                    |> FeatherIcons.toHtml []
                                , text "Import"
                                ]
                            , textarea
                                [ placeholder "Paste data here"
                                , class "textarea textarea-secondary p-2 resize-none leading-tight text-2xs h-16"
                                , classList [ ( "textarea-error", model.importHasErr ) ]
                                , value model.extensionDataToImport
                                , onInput HandleExtensionDataInput
                                ]
                                []
                            ]
                        ]
                    ]
                ]
            , div [ class optionContainer ]
                [ div [ class "flex items-center space-x-4 pr-4" ]
                    [ div [ class "form-control flex-grow" ]
                        [ label [ class "label pt-0", for "override-token" ]
                            [ text "Override Token" ]
                        , input
                            [ class "input input-bordered input-sm w-full"
                            , id "override-token"
                            , onInput HandleOverrideTokenInput
                            , value model.overrideToken
                            ]
                            []
                        ]
                    , a [ class "link link-primary inline-block mt-[1.5rem] text-lg", href "https://www.dropbox.com/admin/stormcrow#/override", onClick OpenToken ] [ text "Get Token" ]
                    ]
                ]
            ]
        ]


view : Model -> Html Msg
view model =
    let
        -- Pretty ugly hack but I couldn't get the widths to be consistent when switching tabs without this
        tabWidthClass =
            "w-full"
    in
    div [ class "flex flex-col h-screen w-screen overflow-hidden", attribute "data-theme" model.theme ] <|
        [ renderHeader model
        , div [ class "h-[81vh] overflow-y-auto px-4 py-2" ] <|
            [ div
                [ class <|
                    if model.activeTab == MainTab then
                        "z-10"

                    else
                        "hidden z-0"
                , class tabWidthClass
                ]
                [ renderMainTab model ]
            , div
                [ class <|
                    if model.activeTab == SettingsTab then
                        "z-10"

                    else
                        "hidden z-0"
                , class tabWidthClass
                ]
                [ renderSettingsTab model ]
            ]
        , renderFooter
        ]
