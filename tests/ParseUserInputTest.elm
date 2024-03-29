module ParseUserInputTest exposing (suite)

import Expect
import ParseUserInput exposing (parseUserInput)
import Test exposing (..)


suite : Test
suite =
    describe "ParseUserInput module"
        [ describe "inputs that are a simple string"
            [ test "single word" <|
                \_ ->
                    Expect.equal
                        (parseUserInput "foo")
                        [ ( "foo", "ON" ) ]
            , test "word with underscores" <|
                \_ ->
                    Expect.equal
                        (parseUserInput "foo_bar")
                        [ ( "foo_bar", "ON" ) ]
            ]
        , describe "inputs that have both feature and variant"
            [ test "feature and variant separated by ':'" <|
                \_ ->
                    Expect.equal
                        (parseUserInput "foo:OFF")
                        [ ( "foo", "OFF" ) ]
            ]
        , describe "inputs that look like a url"
            [ test "query string input" <|
                \_ ->
                    Expect.equal
                        (parseUserInput "?stormcrow_override=foo:bar&stormcrow_override=baz:ON")
                        [ ( "foo", "bar" ), ( "baz", "ON" ) ]
            , test "query string input without the ? at start" <|
                \_ ->
                    Expect.equal
                        (parseUserInput "stormcrow_override=foo:bar&stormcrow_override=baz:ON")
                        [ ( "foo", "bar" ), ( "baz", "ON" ) ]
            , test "invalid query string input" <|
                \_ ->
                    Expect.equal
                        (parseUserInput "?stormcrow_override==foo:bar&&stormcrow_override==baz:ON")
                        []
            , test "partially invalid query string input" <|
                \_ ->
                    Expect.equal
                        (parseUserInput "?stormcrow_override=foo:bar&&stormcrow_override==baz:ON")
                        [ ( "foo", "bar" ) ]
            , test "decoded query string input" <|
                \_ ->
                    Expect.equal
                        (parseUserInput "?stormcrow_override%3Dfoo%253Abar%26stormcrow_override%3Dbaz%253AON")
                        [ ( "foo", "bar" ), ( "baz", "ON" ) ]
            , test "full url input" <|
                \_ ->
                    Expect.equal
                        (parseUserInput "https://www.dropbox.com/preview/somestring?other_params=foobar&stormcrow_override=foo:bar")
                        [ ( "foo", "bar" ) ]
            , test "this full url" <|
                \_ ->
                    Expect.equal
                        (parseUserInput "https://www.dropbox.com/work/Tristan%20Pendergrass?stormcrow_override=foo%3AOFF&stormcrow_override_ttl=3600")
                        [ ( "foo", "OFF" ) ]
            ]
        ]
