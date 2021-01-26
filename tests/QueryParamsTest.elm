module QueryParamsTest exposing (suite)

import Expect exposing (Expectation)
import QueryParams exposing (QueryParam)
import Test exposing (..)
import Url exposing (Url)


suite : Test
suite =
    let
        baseUrl : Url
        baseUrl =
            { protocol = Url.Https
            , host = "dropbox.com"
            , port_ = Nothing
            , path = "/home"
            , query = Just "stormcrow_override=use_foo:ON"
            , fragment = Nothing
            }

        baseParams : List QueryParam
        baseParams =
            QueryParams.fromUrl baseUrl
    in
    describe "QueryParams module"
        [ test "fromUrl and toString work" <|
            \_ ->
                let
                    toString : Maybe String
                    toString =
                        QueryParams.toString baseParams
                in
                Expect.equal toString (Just "stormcrow_override=use_foo:ON")
        , describe "applyOverride"
            [ test "can add a new rule" <|
                \_ ->
                    let
                        modified : List QueryParam
                        modified =
                            QueryParams.applyOverride "use_bar" "M1" baseParams

                        toString : Maybe String
                        toString =
                            QueryParams.toString modified
                    in
                    Expect.equal toString (Just "stormcrow_override=use_foo:ON&stormcrow_override=use_bar:M1")
            , test "can change a rule value" <|
                \_ ->
                    let
                        modified : List QueryParam
                        modified =
                            QueryParams.applyOverride "use_foo" "OFF" baseParams

                        toString : Maybe String
                        toString =
                            QueryParams.toString modified
                    in
                    Expect.equal toString (Just "stormcrow_override=use_foo:OFF")
            ]
        ]
