module ParseUserInput exposing (parseUserInput)

import Url
import Url.Parser exposing ((<?>), Parser)
import Url.Parser.Query


parseUserInput : String -> List ( String, String )
parseUserInput userInput =
    let
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

        parser : Parser (List ( String, String ) -> a) a
        parser =
            Url.Parser.top <?> stormcrowOverridesParser
    in
    Url.fromString ("http://example.com" ++ userInput)
        |> Maybe.andThen (Url.Parser.parse parser)
        |> Maybe.withDefault []
