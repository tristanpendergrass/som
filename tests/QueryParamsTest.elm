module QueryParamsTest exposing (suite)

import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
    describe "QueryParams module"
        [ describe "toString"
            [ test "works" <|
                \_ -> Expect.equal 2 2
            ]
        ]
