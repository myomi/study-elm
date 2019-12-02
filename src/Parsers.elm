module Parsers exposing (Time, newTime, time)

import Parser exposing ((|.), (|=), Parser, andThen, chompWhile, end, getChompedString, problem, run, succeed, symbol)


type alias Time =
    { hour : Int
    , minute : Int
    }


newTime : Int -> Int -> Time
newTime hour minute =
    { hour = hour
    , minute = minute
    }


hourParser : Parser Int
hourParser =
    getChompedString (chompWhile Char.isDigit)
        |> andThen
            (\x ->
                case String.toInt x of
                    Just n ->
                        if n >= 0 && n <= 23 then
                            succeed n

                        else
                            problem ("invalid hour: " ++ x)

                    Nothing ->
                        problem ("invalid hour: " ++ x)
            )


minParser : Parser Int
minParser =
    getChompedString (chompWhile Char.isDigit)
        |> andThen
            (\x ->
                case String.toInt x of
                    Just n ->
                        if n >= 0 && n <= 59 then
                            succeed n

                        else
                            problem ("invalid min: " ++ x)

                    Nothing ->
                        problem ("invalid min: " ++ x)
            )


time : String -> Result (List Parser.DeadEnd) Time
time src =
    let
        timeParser =
            succeed Time
                |= hourParser
                |. symbol ":"
                |= minParser
                |. end
    in
    run timeParser src
