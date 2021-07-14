port module Main exposing (main)

import Browser
import Browser.Dom
import FeatherIcons
import Html exposing (Html, a, button, div, form, h1, h2, input, label, li, option, select, span, text, ul)
import Html.Attributes exposing (checked, class, classList, disabled, for, id, name, placeholder, selected, style, type_, value)
import Html.Events exposing (onBlur, onCheck, onClick, onInput, onSubmit)
import Json.Decode as D
import Json.Encode as E
import Json.Encode.Extra
import List
import QueryParams exposing (QueryParam)
import Task
import Time
import Url exposing (Url)


main : Program ( String, D.Value ) Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


type TtlLength
    = TtlShort
    | TtlMedium
    | TtlLong
    | TtlMax


ttlLengthDecoder : D.Decoder TtlLength
ttlLengthDecoder =
    D.string
        |> D.andThen
            (\ttlLength ->
                case ttlLength of
                    "ttlShort" ->
                        D.succeed TtlShort

                    "ttlMedium" ->
                        D.succeed TtlMedium

                    "ttlLong" ->
                        D.succeed TtlLong

                    "ttlMax" ->
                        D.succeed TtlMax

                    _ ->
                        D.fail "ttlLength not recognized"
            )


ttlLengthEncoder : TtlLength -> E.Value
ttlLengthEncoder ttlLength =
    case ttlLength of
        TtlShort ->
            E.string "ttlShort"

        TtlMedium ->
            E.string "ttlMedium"

        TtlLong ->
            E.string "ttlLong"

        TtlMax ->
            E.string "ttlMax"


type alias TtlConfigObject =
    { length : TtlLength
    , seconds : Int
    , htmlId : String
    , htmlText : String
    }


ttlConfig :
    { short : TtlConfigObject
    , medium : TtlConfigObject
    , long : TtlConfigObject
    , max : TtlConfigObject
    }
ttlConfig =
    { short = { length = TtlShort, seconds = 60 * 5, htmlId = "ttl-short", htmlText = "5 min" }
    , medium = { length = TtlMedium, seconds = 60 * 60, htmlId = "ttl-medium", htmlText = "1 hour" }
    , long = { length = TtlLong, seconds = 60 * 60 * 24, htmlId = "ttl-long", htmlText = "24 hours" }
    , max = { length = TtlMax, seconds = 60 * 60 * 24 * 30, htmlId = "ttl-max", htmlText = "30 days (max)" }
    }


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


legacyToNew : LegacyOverride -> Override
legacyToNew { id, feature, variantSelection, customVariantText } =
    { id = id, feature = feature, variantSelection = variantSelection, customVariantText = customVariantText }



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
    }


type alias LegacyOverride =
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
    | OptionsTab


type alias Model =
    { nonce : Int
    , browserUrl : Maybe Url
    , activeOverrides : List Override
    , inactiveOverrides : List Override
    , archivedOverrides : List Override
    , feature : String
    , featureEditState : FeatureEditState
    , featureFilter : String
    , activeTab : ActiveTab
    , overrideToken : String
    , tokenCreatedAt : Maybe Time.Posix
    , ttlLength : TtlLength
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
            D.map4 Override
                (D.field "id" D.int)
                (D.field "feature" D.string)
                (D.field "variantSelection" (D.map variantSelectionFromString D.string))
                (D.field "customVariantText" D.string)

        legacyOverrideDecoder : D.Decoder LegacyOverride
        legacyOverrideDecoder =
            D.map5 LegacyOverride
                (D.field "id" D.int)
                (D.field "feature" D.string)
                (D.field "variantSelection" (D.map variantSelectionFromString D.string))
                (D.field "customVariantText" D.string)
                (D.field "isSelected" D.bool)

        overridesFromLegacyOverrides : List LegacyOverride -> ( List Override, List Override )
        overridesFromLegacyOverrides legacyOverrides =
            let
                newActiveOverrides =
                    legacyOverrides
                        |> List.filter .isSelected
                        |> List.map legacyToNew

                newInactiveOverrides =
                    legacyOverrides
                        |> List.filter (.isSelected >> not)
                        |> List.map legacyToNew
            in
            ( newActiveOverrides, newInactiveOverrides )

        legacyOverridesDecoder : D.Decoder ( List Override, List Override )
        legacyOverridesDecoder =
            D.map overridesFromLegacyOverrides (D.field "overrides" (D.list legacyOverrideDecoder))

        overridesDecoder : D.Decoder ( List Override, List Override )
        overridesDecoder =
            D.map2 Tuple.pair (D.field "activeOverrides" (D.list overrideDecoder)) (D.field "inactiveOverrides" (D.list overrideDecoder))

        -- We have to use oneOf here to try two different decoders because some clients will have the old format of `overrides` in local storage instead of the current format `(activeOverrides, inactiveOverrides)`
        ( activeOverrides, inactiveOverrides ) =
            D.decodeValue (D.oneOf [ legacyOverridesDecoder, overridesDecoder ]) localStorageData
                |> Result.withDefault ( [], [] )

        archivedOverrides : List Override
        archivedOverrides =
            D.decodeValue (D.field "archivedOverrides" (D.list overrideDecoder)) localStorageData
                |> Result.withDefault []

        overrideToken : String
        overrideToken =
            D.decodeValue (D.field "overrideToken" D.string) localStorageData
                |> Result.withDefault ""

        tokenCreatedAt : Maybe Time.Posix
        tokenCreatedAt =
            D.decodeValue (D.field "tokenCreatedAt" D.int) localStorageData
                |> Result.map (Time.millisToPosix >> Just)
                |> Result.withDefault Nothing

        ttlLength : TtlLength
        ttlLength =
            D.decodeValue (D.field "ttlLength" ttlLengthDecoder) localStorageData
                |> Result.withDefault TtlMedium

        model : Model
        model =
            { nonce = nonce
            , browserUrl = Url.fromString initialBrowserUrl
            , activeOverrides = activeOverrides
            , inactiveOverrides = inactiveOverrides
            , archivedOverrides = archivedOverrides
            , feature = ""
            , featureEditState = NotEditing
            , featureFilter = ""
            , activeTab = MainTab
            , overrideToken = overrideToken
            , tokenCreatedAt = tokenCreatedAt
            , ttlLength = ttlLength
            }
    in
    ( model
    , Task.perform CheckIfTokenExpired Time.now
    )



-- UPDATE


port sendUrl : String -> Cmd msg


port sendToLocalStorage : E.Value -> Cmd msg


port createTab : String -> Cmd msg


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
        , ( "ttlLength", ttlLengthEncoder model.ttlLength )
        ]


makeUrl : TtlLength -> String -> List Override -> Url -> Url
makeUrl ttlLength token overrides oldUrl =
    let
        applyOverrides : List QueryParam -> List QueryParam
        applyOverrides oldQueryParams =
            overrides
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
                    oldQueryParams

        queryTtlMillis =
            case ttlLength of
                TtlShort ->
                    ttlConfig.short.seconds

                TtlMedium ->
                    ttlConfig.medium.seconds

                TtlLong ->
                    ttlConfig.long.seconds

                TtlMax ->
                    ttlConfig.max.seconds

        newQueryParams =
            QueryParams.fromUrl oldUrl
                |> List.filter (QueryParams.isStormcrowParam >> not)
                |> applyOverrides
                |> QueryParams.setTtl (String.fromInt queryTtlMillis)
                |> QueryParams.setToken token

        newUrl : Url
        newUrl =
            { oldUrl | query = QueryParams.toString newQueryParams }
    in
    newUrl


type Msg
    = SetBrowserUrl String
    | HandleFeatureFilterInput String
    | ApplyOverrides
    | SetActiveTab ActiveTab
    | FocusResult (Result Browser.Dom.Error ())
    | OpenGithub
    | OpenToken
    | HandleOverrideTokenInput String
    | SetTokenCreatedAt (Maybe Time.Posix)
    | CheckIfTokenExpired Time.Posix
    | SetTtl TtlLength
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


setFeatureEditState : FeatureEditState -> Model -> Model
setFeatureEditState newFeatureEditState model =
    { model | featureEditState = newFeatureEditState }


addActiveOverride : Override -> Model -> Model
addActiveOverride override model =
    { model | activeOverrides = override :: model.activeOverrides }


addInactiveOverride : Override -> Model -> Model
addInactiveOverride override model =
    { model | inactiveOverrides = override :: model.inactiveOverrides }


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

                newModel : Model
                newModel =
                    model
                        |> replaceOverride override newOverride
            in
            ( newModel, sendToLocalStorage <| encodeModel newModel )

        HandleVariantSelectionInput override selection ->
            let
                newOverride : Override
                newOverride =
                    { override | variantSelection = variantSelectionFromString selection }

                newModel : Model
                newModel =
                    model
                        |> removeOverride override
                        |> addActiveOverride newOverride
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
                            makeUrl model.ttlLength model.overrideToken model.activeOverrides oldUrl
                    in
                    ( model, sendUrl <| Url.toString newUrl )

        SetActiveTab activeTab ->
            ( { model | activeTab = activeTab }, Cmd.none )

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

        SetTtl ttlLength ->
            let
                newModel : Model
                newModel =
                    { model | ttlLength = ttlLength }
            in
            ( newModel, sendToLocalStorage <| encodeModel newModel )

        HandleAddOverrideFeatureInput feature ->
            ( { model | feature = feature }, Cmd.none )

        HandleAddOverrideSubmit ->
            let
                newOverride : Override
                newOverride =
                    Override model.nonce model.feature OnVariant ""

                newModel : Model
                newModel =
                    model
                        |> addActiveOverride newOverride
                        |> (\oldModel -> { oldModel | nonce = model.nonce + 1, feature = "" })
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
                            model
                                |> replaceOverride previousOverride { previousOverride | feature = getDraftValue draft }
                                |> setFeatureEditState NotEditing
                    in
                    ( newModel, sendToLocalStorage <| encodeModel newModel )

                ( Just override, NotEditing ) ->
                    ( { model | featureEditState = Editing override (DraftValue override.feature) (OriginalValue override.feature) }
                    , Browser.Dom.focus featureInputId |> Task.attempt FocusResult
                    )

                ( Just override, Editing previousOverride _ originalValue ) ->
                    let
                        newFeatureEditState =
                            Editing override (DraftValue override.feature) (OriginalValue override.feature)

                        newModel =
                            model
                                |> replaceOverride previousOverride { previousOverride | feature = getOriginalValue originalValue }
                                |> setFeatureEditState newFeatureEditState
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
                    let
                        newModel =
                            model
                                |> replaceOverride override { override | feature = getOriginalValue originalValue }
                                |> setFeatureEditState NotEditing
                    in
                    ( newModel, Cmd.none )

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
                        |> addToList override
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



-- The common classes for other tooltip utilities. Not intended to be used directly.


tooltipContent : String
tooltipContent =
    "tooltip-text absolute z-50 bg-gray-900 text-gray-100 text-sm py-1 px-2 rounded-sm transition duration-200 delay-300"


tooltipText : String
tooltipText =
    tooltipContent ++ " whitespace-nowrap"


tooltipBlockText : String
tooltipBlockText =
    tooltipContent ++ " break-words"


iconButton : String
iconButton =
    "icon-button p-1 rounded group focus:outline-none hover:bg-gray-100"


primaryButton : String
primaryButton =
    "primary-button bg-blue-500 text-gray-100 p-2 rounded-lg font-bold antialiased hover:bg-blue-400 cursor-pointer capitalize"


primaryButtonDisabled : String
primaryButtonDisabled =
    "cursor-not-allowed opacity-50 hover:bg-blue-500"


linkText : String
linkText =
    "text-blue-500 hover:text-blue-400 font-bold uppercase px-3 py-1 text-xs mr-1 mb-1 cursor-pointer"


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


renderOverride : Bool -> FeatureEditState -> Override -> Html Msg
renderOverride isActive featureEditState override =
    let
        fadeIfInactive =
            class <|
                if isActive then
                    ""

                else
                    "opacity-50"

        featureText =
            div [ class tooltip ]
                [ div [ class "truncate", fadeIfInactive ] [ text override.feature ]
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

        toggleButton =
            button
                [ class iconButton
                , class "opacity-0 group-hover:opacity-100"
                , onClick (ToggleSelectOverride override (not isActive))
                ]
                [ if isActive then
                    FeatherIcons.minusCircle
                        |> FeatherIcons.withSize 16
                        |> FeatherIcons.withClass "opacity-0 group-hover:opacity-100 text-red-500"
                        |> FeatherIcons.toHtml []

                  else
                    FeatherIcons.plusCircle
                        |> FeatherIcons.withSize 16
                        |> FeatherIcons.withClass "opacity-0 group-hover:opacity-100 text-blue-500"
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
    div [ class "flex items-center space-x-1 group" ]
        [ toggleButton
        , div [ class "flex-grow flex justify-between" ]
            -- [ div [ classList [ ( titleColor, isActive ) ], style "width" (String.fromInt halfWidth ++ "px") ] [ labelOrInput ]
            [ div [ style "width" (String.fromInt halfWidth ++ "px") ] [ labelOrInput ]
            , div [ fadeIfInactive ] [ text ":" ]
            , div
                [ class "flex justify-end space-x-1"
                , style "width" (String.fromInt halfWidth ++ "px")
                , fadeIfInactive
                ]
                [ customVariantInput
                , select
                    [ onInput (HandleVariantSelectionInput override), classList [ ( titleColor, isActive ) ] ]
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
        , h2
            [ class tabClasses
            , classList [ ( activeTabClasses, model.activeTab == OptionsTab ), ( inactiveTabClasses, model.activeTab /= OptionsTab ) ]
            , onClick (SetActiveTab OptionsTab)
            ]
            [ text "Options" ]
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
            List.isEmpty model.activeOverrides
    in
    button
        [ class primaryButton
        , classList [ ( primaryButtonDisabled, isDisabled ) ]
        , onClick ApplyOverrides
        , disabled isDisabled
        ]
        [ div [ class "flex items-center space-x-1" ]
            [ FeatherIcons.zap
                |> FeatherIcons.withClass "inline-block"
                |> FeatherIcons.withSize 16
                |> FeatherIcons.toHtml []
            , span [ class "uppercase" ] [ text "Override" ]
            ]
        ]


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

        listHeaderClasses =
            classList
                [ ( "mt-2 mb-1 text-xl", True )
                , ( "hidden", List.isEmpty model.inactiveOverrides )
                ]
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
                    [ class "flex-grow overflow-y-auto " ]
                    (if List.isEmpty model.activeOverrides && List.isEmpty model.inactiveOverrides then
                        [ renderAddOverride model
                        , div [ class "w-full mt-32 text-center" ] [ text "No overrides exist." ]
                        ]

                     else
                        [ renderAddOverride model
                        , div [ listHeaderClasses ] [ text "Active" ]
                        , div [ class "space-y-0.5" ]
                            (model.activeOverrides
                                |> List.filter (.feature >> matchString model.featureFilter)
                                |> List.map (renderOverride True model.featureEditState)
                            )
                        , div [ listHeaderClasses ] [ text "Inactive" ]
                        , div [ class "space-y-0.5" ]
                            (model.inactiveOverrides
                                |> List.filter (.feature >> matchString model.featureFilter)
                                |> List.map (renderOverride False model.featureEditState)
                            )
                        ]
                    )
                , renderFooter
                ]

        ArchiveTab ->
            div [ class bodyClasses ]
                [ renderHeader
                , renderTabs model
                , if List.isEmpty model.archivedOverrides then
                    div [ class "w-full mt-32 text-center h-full" ] [ text "Archive is empty." ]

                  else
                    div [ class "overflow-y-scroll h-full" ]
                        (List.map
                            renderArchivedOverride
                            model.archivedOverrides
                        )
                , renderFooter
                ]

        OptionsTab ->
            let
                tokenHelpIcon =
                    div [ class tooltip ]
                        [ FeatherIcons.helpCircle
                            |> FeatherIcons.withSize 16
                            |> FeatherIcons.toHtml []
                        , div [ class tooltipBlockText, class "w-72" ]
                            [ span [] [ text "A token is necessary if:" ]
                            , ul [ class "list-disc list-inside" ]
                                [ li [] [ text "using staging/prod AND" ]
                                , li [] [ text "using a non-Dropbox account." ]
                                ]
                            , span [] [ text "You must also be on the corporate VPN. A given token lasts for 24 hours and SOM will clear this field after that time." ]
                            ]
                        ]

                ttlHelpIcon =
                    div [ class tooltip ]
                        [ FeatherIcons.helpCircle
                            |> FeatherIcons.withSize 16
                            |> FeatherIcons.toHtml []
                        , div [ class tooltipBlockText, class "w-64" ]
                            [ span [] [ text "Time to Live (TTL) is the amount of time that a Stormcrow override will stay active once set." ]
                            ]
                        ]

                tokenPlaceholder =
                    "BQ8OS6e8J... *OR* &override_token=BQ8OS6e8J..."

                ttlRadioOption : TtlConfigObject -> Html Msg
                ttlRadioOption { htmlId, length, htmlText } =
                    div [ class "flex space-x-1 cursor-pointer items-center" ]
                        [ input [ type_ "radio", id htmlId, name "override-ttl", checked <| model.ttlLength == length, onCheck (\_ -> SetTtl length) ] []
                        , label [ for htmlId ] [ text htmlText ]
                        ]

                optionContainer =
                    "flex-col space-y-1 w-full border border-gray-700 py-4 px-2"
            in
            div [ class bodyClasses ]
                [ renderHeader
                , renderTabs model
                , div [ class "flex-col w-100 items-start my-4 space-y-4 h-full" ]
                    [ div [ class optionContainer ]
                        [ label [ class "flex space-x-1 items-center", for "override-token" ]
                            [ span [ class "text-xs font-medium" ] [ text "Override Token" ]
                            , span [] [ tokenHelpIcon ]
                            ]
                        , div [ class "flex items-center space-x-1" ]
                            [ div [ class "flex-grow" ]
                                [ input
                                    [ class underlineInput
                                    , class <|
                                        if model.overrideToken == "" then
                                            "italic"

                                        else
                                            ""
                                    , id "override-token"
                                    , onInput HandleOverrideTokenInput
                                    , value model.overrideToken
                                    , placeholder tokenPlaceholder
                                    ]
                                    []
                                ]
                            , a [ class linkText, onClick OpenToken ] [ text "Get Token" ]
                            ]
                        ]
                    , div [ class optionContainer ]
                        [ label [ class "flex space-x-1 items-center", for "override-ttl" ]
                            [ span [ class "text-xs font-medium" ] [ text "Override Time to Live (TTL)" ]
                            , span [] [ ttlHelpIcon ]
                            ]
                        , div [ class "flex space-x-4 items-center" ]
                            [ ttlRadioOption ttlConfig.short
                            , ttlRadioOption ttlConfig.medium
                            , ttlRadioOption ttlConfig.long
                            , ttlRadioOption ttlConfig.max
                            ]
                        ]
                    ]
                , renderFooter
                ]
