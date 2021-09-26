module ParseUserInput exposing (parseUserInput)

import Maybe.Extra
import Url
import Url.Parser exposing ((<?>), Parser)
import Url.Parser.Query



-- See the unit tests of this function for clearer picture of what it can parse


parseUserInput : String -> List ( String, String )
parseUserInput userInput =
    Maybe.Extra.orList
        [ parseQueryStringInput userInput
        , parseFullUrlInput userInput
        ]
        |> Maybe.withDefault [ ( "dont", "gethere" ) ]



-- helpers


stormcrowOverridesParser : Url.Parser.Query.Parser (List ( String, String ))
stormcrowOverridesParser =
    Url.Parser.Query.custom "stormcrow_override"
        (List.filterMap
            (\value ->
                case String.split ":" value of
                    [ feature, variant ] ->
                        Just ( feature, variant )

                    _ ->
                        Nothing
            )
        )


parseQueryStringInput : String -> Maybe (List ( String, String ))
parseQueryStringInput userInput =
    let
        parser : Parser (List ( String, String ) -> a) a
        parser =
            Url.Parser.top <?> stormcrowOverridesParser
    in
    userInput
        |> Url.percentDecode
        |> Maybe.andThen (\decodedInput -> Url.fromString ("http://example.com" ++ decodedInput))
        |> Maybe.andThen (\url -> Url.Parser.parse parser url)


parseFullUrlInput : String -> Maybe (List ( String, String ))
parseFullUrlInput userInput =
    let
        parser : Parser (List ( String, String ) -> a) a
        parser =
            Url.Parser.top <?> stormcrowOverridesParser
    in
    userInput
        |> Url.percentDecode
        |> Maybe.andThen (\decodedInput -> Url.fromString decodedInput)
        |> Maybe.map (\url -> { url | path = "" })
        |> Maybe.andThen (Url.Parser.parse parser)
