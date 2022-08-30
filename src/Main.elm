port module Main exposing (main)

-- import Parser exposing (Parser)

import Browser
import Browser.Dom
import FeatherIcons
import Html exposing (Html, a, button, div, form, h1, input, label, li, option, select, span, text, ul)
import Html.Attributes exposing (attribute, checked, class, classList, disabled, for, id, name, placeholder, selected, style, type_, value)
import Html.Events exposing (onCheck, onClick, onInput, onSubmit)
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


queryTtlSeconds : TtlLength -> Int
queryTtlSeconds ttlLength =
    case ttlLength of
        TtlShort ->
            ttlConfig.short.seconds

        TtlMedium ->
            ttlConfig.medium.seconds

        TtlLong ->
            ttlConfig.long.seconds

        TtlMax ->
            ttlConfig.max.seconds


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
    { id = id, feature = feature, variantSelection = variantSelection, customVariantText = customVariantText }



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
            , feature = Nothing
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
        , ( "ttlLength", ttlLengthEncoder model.ttlLength )
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


makeQueryString : TtlLength -> String -> List Override -> String
makeQueryString ttlLength token overrides =
    let
        newQueryParams =
            []
                |> applyOverrides overrides
                |> QueryParams.setTtl (String.fromInt (queryTtlSeconds ttlLength))
                |> QueryParams.setToken token
    in
    QueryParams.toString newQueryParams
        |> Maybe.map (\str -> "?" ++ str)
        |> Maybe.withDefault ""


makeUrl : TtlLength -> String -> List Override -> Url -> Url
makeUrl ttlLength token overrides oldUrl =
    let
        newQueryParams =
            QueryParams.fromUrl oldUrl
                |> List.filter (QueryParams.isStormcrowParam >> not)
                |> applyOverrides overrides
                |> QueryParams.setTtl (String.fromInt (queryTtlSeconds ttlLength))
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
    | SetTtl TtlLength
    | Export
      -- Add Override
    | ToggleFeatureInput
    | HandleAddOverrideFeatureInput String
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
        Override id featureName OnVariant ""

    else if String.toUpper value == "OFF" then
        Override id featureName OffVariant ""

    else
        Override id featureName CustomVariant value


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
    { model | inactiveOverrides = override :: model.inactiveOverrides }


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

        Export ->
            ( model, writeToClipboard <| makeQueryString model.ttlLength model.overrideToken model.activeOverrides )

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
                        |> addToList override
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


iconButton : String
iconButton =
    "icon-button p-1 rounded group focus:outline-none hover:bg-gray-100"


linkText : String
linkText =
    "text-blue-500 hover:text-blue-400 font-bold uppercase px-3 py-1 text-xs mr-1 mb-1 cursor-pointer"


featureInputId : String
featureInputId =
    "feature-input"



-- renderClosedAddOverrideRow : Html Msg
-- renderClosedAddOverrideRow =
--     div [ class "flex w-full h-9 items-center space-x-2" ]
--         [ overrideAddToggleButton { isVisible = True, mode = Off }
--         , div
--             [ class "cursor-pointer"
--             , onClick (ToggleFeatureInput True)
--             ]
--             [ text "Add feature" ]
--         ]


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


renderAddOverride : { feature : Maybe String } -> Html Msg
renderAddOverride { feature } =
    form [ onSubmit <| HandleAddOverrideSubmit { keepOpen = False }, class "flex flex-col space-y-1" ]
        [ div [ class "flex flex-col w-full space-y-2" ]
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
                                , class "input input-xs input-bordered w-full"
                                , onInput HandleAddOverrideFeatureInput
                                ]
                                []

                        Nothing ->
                            div
                                [ class "font-semibold cursor-pointer hover:text-primary"
                                , onClick ToggleFeatureInput
                                ]
                                [ text "Add feature" ]
                    ]
                , case feature of
                    Just featureText ->
                        div [ class "flex space-x-1" ]
                            [ div [ class "tooltip tooltip-left", attribute "data-tip" "Add Override" ]
                                [ button
                                    [ class "btn btn-sm btn-square btn-outline btn-primary"
                                    , disabled <| String.isEmpty featureText
                                    , onClick <| HandleAddOverrideSubmit { keepOpen = False }
                                    ]
                                    [ FeatherIcons.check
                                        |> FeatherIcons.withSize 12
                                        |> FeatherIcons.toHtml []
                                    ]
                                ]
                            , div [ class "tooltip tooltip-left", attribute "data-tip" "Add and keep open" ]
                                [ button
                                    [ class "btn btn-sm btn-outline gap-2 btn-secondary"
                                    , disabled <| String.isEmpty featureText
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

                    Nothing ->
                        div [] []
                ]
            ]
        , div
            [ class "w-full transition-height overflow-hidden flex space-x-2"
            , classList [ ( infoBoxHeight, Maybe.Extra.isJust feature ), ( "h-0", Maybe.Extra.isNothing feature ) ]
            ]
            [ overrideAddToggleButton { isVisible = False, isAdd = True }
            , renderInfoBox
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
    div [ class "flex w-full h-9 items-center space-x-2" ]
        [ overrideCheckbox { isChecked = True, handleCheck = ToggleSelectOverride override }
        , div [ class "flex-grow" ]
            [ input
                [ type_ "text"
                , value override.feature
                , class "input input-bordered input-xs w-full"
                , onInput (HandleFeatureInput override)
                ]
                []
            ]
        , div [ class "flex-grow", classList [ ( "invisible", override.variantSelection /= CustomVariant ) ] ]
            [ input
                [ type_ "text"
                , id <| domIdForCustomVariantInput override
                , value override.customVariantText
                , class "input input-bordered input-xs w-full"
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
    div [ class "flex w-full h-9 items-center space-x-2" ]
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


renderTabs : Model -> Html Msg
renderTabs model =
    div [ class "tabs" ]
        [ a
            [ class "tab tab-bordered tab-md"
            , class <|
                if model.activeTab == MainTab then
                    "tab-active"

                else
                    ""
            , onClick (SetActiveTab MainTab)
            ]
            [ text "Main" ]
        , a
            [ class "tab tab-bordered tab-md"
            , class <|
                if model.activeTab == SettingsTab then
                    "tab-active"

                else
                    ""
            , onClick (SetActiveTab SettingsTab)
            ]
            [ text "Settings" ]
        ]


renderArchivedOverride : Override -> Html Msg
renderArchivedOverride override =
    div [ class "flex items-center" ]
        [ div [ class "tooltip", attribute "data-tip" "Unarchive" ]
            [ button [ class iconButton, onClick (Unarchive override) ]
                [ FeatherIcons.rotateCcw
                    |> FeatherIcons.withSize 12
                    |> FeatherIcons.withClass "text-blue-500"
                    |> FeatherIcons.toHtml []
                ]
            ]
        , div [ class "flex-grow truncate" ] [ text override.feature ]
        , div [ class "tooltip", attribute "data-tip" "Permanently Delete" ]
            [ button [ class iconButton, onClick (Delete override) ]
                [ FeatherIcons.trash2
                    |> FeatherIcons.withSize 12
                    |> FeatherIcons.withClass "text-red-500"
                    |> FeatherIcons.toHtml []
                ]
            ]
        ]


renderHeader : Html Msg
renderHeader =
    h1 [ class "text-xl text-left font-extrabold" ] [ text "Stormcrow Override Manager" ]


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
                , onClick Export
                , disabled isDisabled
                ]
                [ FeatherIcons.clipboard
                    |> FeatherIcons.withSize 16
                    |> FeatherIcons.toHtml []
                , text "Copy"
                ]
            ]
        , div [ class "tooltip tooltip-left", attribute "data-tip" "Settings" ]
            [ button [ class "btn btn-square btn-ghost btn-sm", onClick <| SetActiveTab SettingsTab ]
                [ FeatherIcons.settings
                    |> FeatherIcons.withSize 16
                    |> FeatherIcons.toHtml []
                ]
            ]
        ]


renderFooter : Html Msg
renderFooter =
    div [ class "flex justify-between items-center" ]
        [ div [ class "text-gray-500" ] [ text "Feedback? Message @tristanp" ]
        , div [ class "flex items-center space-x-1" ]
            [ div [ class "text-gray-500" ] [ text "v2.6" ]
            , button [ class iconButton, class "flex items-center space-x-1 py-0", onClick OpenGithub ]
                [ div [] [ text "GitHub" ]
                , FeatherIcons.externalLink
                    |> FeatherIcons.withSize 12
                    |> FeatherIcons.toHtml []
                ]
            ]
        ]


view : Model -> Html Msg
view model =
    let
        bodyClasses =
            "flex flex-col space-y-4 h-full w-screen p-4"
    in
    case model.activeTab of
        MainTab ->
            div [ class bodyClasses ]
                [ renderHeader
                , div [ class "flex justify-left my-2" ]
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

        SettingsTab ->
            let
                -- tokenHelpIcon =
                --     div [ class "tooltip", attribute "data-tip" "kk" ]
                --         [ FeatherIcons.helpCircle
                --             |> FeatherIcons.withSize 16
                --             |> FeatherIcons.toHtml []
                --         , div [ class tooltipBlockText, class "w-72" ]
                --             [ span [] [ text "A token is necessary if:" ]
                --             , ul [ class "list-disc list-inside" ]
                --                 [ li [] [ text "using staging/prod AND" ]
                --                 , li [] [ text "using a non-Dropbox account." ]
                --                 ]
                --             , span [] [ text "You must also be on the corporate VPN. A given token lasts for 24 hours and SOM will clear this field after that time." ]
                --             ]
                --         ]
                ttlHelpIcon =
                    div [ class "tooltip", attribute "data-tip" "Time to Live (TTL) is the amount of time that a Stormcrow override will stay active once set." ]
                        [ FeatherIcons.helpCircle
                            |> FeatherIcons.withSize 16
                            |> FeatherIcons.toHtml []
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
                , div [ class "flex w-full justify-between items-center" ]
                    [ span [ class "text-lg font-bold" ] [ text "Settings" ]
                    , button [ class "btn btn-sm btn-ghost gap-2", onClick <| SetActiveTab MainTab ]
                        [ FeatherIcons.arrowLeftCircle
                            |> FeatherIcons.withSize 16
                            |> FeatherIcons.toHtml []
                        , text "Done"
                        ]
                    ]
                , div [ class "flex-col w-100 items-start my-4 space-y-4 h-full" ]
                    [ div [ class optionContainer ]
                        [ label [ class "flex space-x-1 items-center", for "override-token" ]
                            [ span [ class "text-xs font-medium" ] [ text "Override Token" ]

                            -- , span [] [ tokenHelpIcon ]
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
