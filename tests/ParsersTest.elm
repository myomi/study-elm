module ParsersTest exposing (suite)

-- import Fuzz exposing (Fuzzer, int, list, string)

import Expect
import Parsers exposing (time)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Parse from String to Time"
        [ test "10:00 is OK" <|
            \_ ->
                Expect.equal (time "10:00") (Ok { hour = 10, minute = 0 })
        , test "not number" <|
            \_ ->
                Expect.err (time "a")
        , test "24:00 is Err" <|
            \_ ->
                Expect.err (time "24:00")
        , test "23:60 is Err" <|
            \_ ->
                Expect.err (time "23:60")
        ]
