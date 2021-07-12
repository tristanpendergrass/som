module QueryParams exposing (QueryParam, applyOverride, fromUrl, setTtl, toString)

import Regex
import Url exposing (Url)


type alias Feature =
    String


type alias Variant =
    String


type QueryParam
    = StormcrowParam Feature Variant
    | OtherParam String


colon : Regex.Regex
colon =
    Maybe.withDefault Regex.never <|
        Regex.fromString ":|%3A"


{-| Decode a string as a query param.
e.g.

  - singleFromString "foo=bar" == OtherParam "foo=bar"
  - singleFromString "stormcrow\_override=foo:bar" == StormcrowParam "foo" "bar"

-}
singleFromString : String -> QueryParam
singleFromString param =
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


fromUrl : Url -> List QueryParam
fromUrl url =
    case url.query of
        Nothing ->
            []

        Just query ->
            query
                |> String.split "&"
                |> List.map singleFromString


singleToString : QueryParam -> String
singleToString param =
    case param of
        OtherParam value ->
            value

        StormcrowParam feature variant ->
            "stormcrow_override=" ++ feature ++ ":" ++ variant


{-| Checks a list of query params for a certain feature, and changes its variant if found.
If not found, constructs and appends a new StormcrowOverride query to the list.
-}
applyOverride : Feature -> Variant -> List QueryParam -> List QueryParam
applyOverride overrideFeature overrideVariant oldQueryParams =
    let
        ( overridePresent, newQueryParams ) =
            List.foldr
                (\queryParam ->
                    \( found, accQueryParams ) ->
                        case queryParam of
                            OtherParam _ ->
                                ( found, queryParam :: accQueryParams )

                            StormcrowParam feature _ ->
                                if feature == overrideFeature then
                                    ( True, StormcrowParam feature overrideVariant :: accQueryParams )

                                else
                                    ( found, queryParam :: accQueryParams )
                )
                ( False, [] )
                oldQueryParams
    in
    if overridePresent then
        newQueryParams

    else
        List.append oldQueryParams [ StormcrowParam overrideFeature overrideVariant ]


toString : List QueryParam -> Maybe String
toString queryParams =
    case queryParams of
        [] ->
            Nothing

        params ->
            params
                |> List.map singleToString
                |> String.join "&"
                |> Just


isTtlParam : QueryParam -> Bool
isTtlParam queryParam =
    case queryParam of
        StormcrowParam _ _ ->
            False

        OtherParam value ->
            String.startsWith "stormcrow_override_ttl=" value


setTtl : String -> List QueryParam -> List QueryParam
setTtl ttlValue queryParams =
    let
        -- We want to remove the stormcrow_override_ttl param because we'll be adding our own to the url
        paramsWithTtlRemoved =
            queryParams
                |> List.filter (isTtlParam >> not)

        ttlParam : QueryParam
        ttlParam =
            OtherParam ("stormcrow_override_ttl=" ++ ttlValue)
    in
    List.concat [ paramsWithTtlRemoved, [ ttlParam ] ]
