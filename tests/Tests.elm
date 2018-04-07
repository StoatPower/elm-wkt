module Tests exposing (..)

import WellKnown exposing (..)
import GeoJson exposing (Geometry(..), Position)
import Combine as C
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


testParser : C.Parser () a -> String -> a -> Expectation
testParser parser input expectedResult =
    case C.parse parser input of
        Ok (_, _, result) ->
            Expect.equal result expectedResult

        Err (_, _, errors) ->
            Expect.fail (String.join "\n" errors)


suite : Test
suite =
    describe "WKT to GeoJSON Position parser"
        [ test "integer position with no altitude" <|
            \_ ->
                testParser positionParser "1 1" (1.0, 1.0, 0.0)
        , test "float position with no altitude" <|
            \_ ->
                testParser positionParser "1.1 1.1" (1.1, 1.1, 0.0)
        , test "integer position with altitude" <|
            \_ ->
                testParser positionParser "1 1 2" (1.0, 1.0, 2.0)
        , test "float position with altitude" <|
            \_ ->
                testParser positionParser "1.1 1.1 2.2"  (1.1, 1.1, 2.2)
        ]