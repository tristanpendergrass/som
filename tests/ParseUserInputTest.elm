module ParseUserInputTest exposing (suite)

import Expect exposing (Expectation)
import ParseUserInput exposing (parseUserInput)
import Test exposing (..)
import Url exposing (Url)


suite : Test
suite =
    describe "ParseUserInput module"
        [ test "query string input" <|
            \_ ->
                Expect.equal
                    (parseUserInput "?stormcrow_override=foo:bar&stormcrow_override=baz:ON")
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
        ]
