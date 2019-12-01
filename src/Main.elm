module Main exposing (main)

import Browser
import Html exposing (Html, button, div, form, input, label, li, text, ul)
import Html.Attributes exposing (class, disabled, for, id, type_, value)
import Html.Events exposing (onClick, onInput)
import Parser as P exposing ((|.), (|=))


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }



-- MODEL


{-| 1日の勤怠情報
-}
type alias Attendance =
    { startTime : String -- 開始時刻
    , endTime : String -- 終了時刻
    }


type alias Input =
    { value : String
    , error : Error
    }


{-| 入力フォーム
-}
type alias Form =
    { startTime : Input
    , endTime : Input
    }


{-| 入力エラー
-}
type Error
    = Required
    | Invalid
    | None


type alias Model =
    { attendances : List Attendance -- 保存された勤怠情報一覧
    , input : Attendance -- 入力中の勤怠情報
    , form : Form -- 入力フォーム
    }


init : Model
init =
    { attendances =
        []
    , input =
        { startTime = ""
        , endTime = ""
        }
    , form =
        { startTime =
            { value = ""
            , error = None
            }
        , endTime =
            { value = ""
            , error = None
            }
        }
    }


{-| モデルのinputフィールドを更新する
-}
setInput : Attendance -> Model -> Model
setInput newValue model =
    { model | input = newValue }


{-| モデルのformフィールドを更新する
-}
setForm : Form -> Model -> Model
setForm newValue model =
    { model | form = newValue }


{-| モデルのattendances フィル―ドを更新する
-}
setAttendances : List Attendance -> Model -> Model
setAttendances attendances model =
    { model | attendances = attendances }


isValidForm : Form -> Bool
isValidForm form =
    form.startTime.error == None && form.endTime.error == None


type alias Time =
    { hour : Int
    , minute : Int
    }


hourParser : String -> P.Parser Int
hourParser x =
    case String.toInt x of
        Just n ->
            if n >= 0 && n <= 23 then
                P.succeed n

            else
                P.problem ("invalid min: " ++ x)

        Nothing ->
            P.problem ("invalid min: " ++ x)


minParser : String -> P.Parser Int
minParser x =
    case String.toInt x of
        Just n ->
            if n >= 0 && n <= 59 then
                P.succeed n

            else
                P.problem ("invalid min: " ++ x)

        Nothing ->
            P.problem ("invalid min: " ++ x)


{-| 時刻フォーマットのバリデーション
00:00 ～ 23:59
-}
isTime : String -> Bool
isTime src =
    let
        timeParser =
            P.succeed Time
                |= (P.getChompedString (P.chompWhile Char.isDigit)
                        |> P.andThen hourParser
                   )
                |. P.symbol ":"
                |= (P.getChompedString (P.chompWhile Char.isDigit)
                        |> P.andThen minParser
                   )
                |. P.end
    in
    case P.run timeParser src of
        Ok _ ->
            True

        Err _ ->
            False



-- UPDATE


type Msg
    = InputStartTime String -- 開始を入力
    | InputEndTime String -- 終了を入力
    | SaveAttendance -- 保存


update : Msg -> Model -> Model
update msg model =
    case msg of
        -- start time 入力
        InputStartTime newValue ->
            let
                -- エラーチェック
                newError =
                    if String.length newValue == 0 then
                        Required

                    else if not (isTime newValue) then
                        Invalid

                    else
                        None

                -- フォーム更新
                newForm =
                    { startTime =
                        { value = newValue
                        , error = newError
                        }
                    , endTime = model.form.endTime
                    }
            in
            model |> setForm newForm

        -- end time 入力
        InputEndTime newValue ->
            let
                -- エラーチェック
                newError =
                    if String.length newValue == 0 then
                        Required

                    else if not (isTime newValue) then
                        Invalid

                    else
                        None

                -- フォーム更新
                newForm =
                    { startTime = model.form.startTime
                    , endTime =
                        { value = newValue
                        , error = newError
                        }
                    }
            in
            model |> setForm newForm

        -- 保存ボタンクリック
        SaveAttendance ->
            let
                newAttendances =
                    model.input :: model.attendances

                newInput =
                    { startTime = "", endTime = "" }
            in
            model
                |> setAttendances newAttendances
                |> setInput newInput



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewForm model
        , viewAttendances model
        ]


{-| 勤怠情報をリスト表示する
-}
viewAttendances : Model -> Html Msg
viewAttendances model =
    ul []
        (List.map
            (\a -> li [] [ text (a.startTime ++ "～" ++ a.endTime) ])
            model.attendances
        )


{-| 勤怠情報の入力フォームを表示する
-}
viewForm : Model -> Html Msg
viewForm model =
    form []
        [ div []
            [ label [ for "startDate" ] [ text "開始" ]
            , input
                [ value model.form.startTime.value
                , onInput InputStartTime
                , id "startDate"
                , class
                    (case model.form.startTime.error of
                        Required ->
                            "error--required"

                        Invalid ->
                            "error--invalid"

                        None ->
                            ""
                    )
                ]
                []
            ]
        , div []
            [ label [ for "endDate" ] [ text "終了" ]
            , input
                [ value model.form.endTime.value
                , onInput InputEndTime
                , id "endDate"
                , class
                    (case model.form.endTime.error of
                        Required ->
                            "error--required"

                        Invalid ->
                            "error--invalid"

                        None ->
                            ""
                    )
                ]
                []
            ]
        , button
            [ type_ "button"
            , onClick SaveAttendance
            , disabled (not (isValidForm model.form))
            ]
            [ text "Save" ]
        ]
