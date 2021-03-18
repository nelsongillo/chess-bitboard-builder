module Main exposing (..)

import Browser
import Css
import Html.Styled exposing (Html, button, div, p, text, toUnstyled)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import UInt64
import UInt64.Digits as Digits
import Css


-----------------------------------------------------------------------------------------------------------------------
--  Types
-----------------------------------------------------------------------------------------------------------------------
type alias Model = UInt64.UInt64

type alias Square = Int

type Msg
    = SquarePressed Square
    | Invert
    | Reset


-----------------------------------------------------------------------------------------------------------------------
--  Setup
-----------------------------------------------------------------------------------------------------------------------
main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = \_ -> Sub.none
        }


-- init Model
init : () -> ( Model, Cmd Msg )
init _ =
    ( UInt64.zero
    , Cmd.none
    )


-----------------------------------------------------------------------------------------------------------------------
--  Update
-----------------------------------------------------------------------------------------------------------------------
-- update Model
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SquarePressed sq -> squarePressed sq model
        Invert -> (UInt64.complement model, Cmd.none)
        Reset -> init ()


-----------------------------------------------------------------------------------------------------------------------
--  Render
-----------------------------------------------------------------------------------------------------------------------
-- view
view : Model -> Html Msg
view model =
    div [ css [Css.textAlign Css.center ]]
        [ p [] [board model 400.0]
        , p []  [ button [ onClick Reset ] [ text "Reset" ]
                , button [ onClick Invert ] [ text "Invert" ]
                ]
        , p [] [ text ("0x" ++ UInt64.toHexString model)  ]
        , p [] [
            text (model
                    |> UInt64.toDigits Digits.binary
                    |> Digits.pad 64 '0'
                    |> Digits.groupToString 4 ' ')
            ]
        ]

-- Draw Board
board : Model -> Float -> Html Msg
board model size =
    div
        [ css
            [ Css.width (Css.px size)
            , Css.height (Css.px size)
            , Css.position Css.relative
            , Css.marginLeft Css.auto
            , Css.marginRight Css.auto
            ]
        ]
        (List.map
            (\s ->
                square
                    (squareToCoordinates s)
                    (UInt64.and (sqMask s) model |> UInt64.isZero |> not)
                    (size / 8)
                    (SquarePressed s)
            )
            <| List.range 0 63
        )


-- draw Square
square : ( Int, Int ) -> Bool -> Float -> Msg -> Html Msg
square ( col, row ) set sqSize msg =
    let border = 1
        size = sqSize - (2 * border) 
    in div
        [ css
            [ Css.backgroundColor
                (if modBy 2 (col + row) == 0
                    then Css.rgb 230 204 179
                    else Css.rgb 115 77 38
                )
            , Css.border3 (Css.px border) Css.solid (Css.rgb 0 0 0)
            , Css.position Css.absolute
            , Css.top (Css.px (toFloat row * sqSize))
            , Css.left (Css.px (toFloat col * sqSize))
            , Css.width (Css.px size)
            , Css.height (Css.px size)
            ]
        , onClick msg
        ]
        [ if set 
                then div
                    [ css
                        [ Css.width  <| Css.px size
                        , Css.height <| Css.px size
                        , Css.backgroundColor (Css.rgb 255 0 0)
                        , Css.borderRadius (Css.pct 50)
                        ]
                    ]
                    [ text <| squareToString (col, row) ]
                else div
                    []
                    [ text <| squareToString (col, row) ]
        ]


-----------------------------------------------------------------------------------------------------------------------
--  Player Interaction
-----------------------------------------------------------------------------------------------------------------------
-- Square Pressed, update Board
squarePressed : Square -> Model -> ( Model, Cmd Msg )
squarePressed sq model =
    ( UInt64.xor (sqMask sq) model
    , Cmd.none
    )


-----------------------------------------------------------------------------------------------------------------------
--  Utility
-----------------------------------------------------------------------------------------------------------------------
-- get Coordinates from square
squareToCoordinates : Square -> ( Int, Int )
squareToCoordinates sq =
    let file = remainderBy 8 sq
        rank = 7 - (sq // 8)
    in (file, rank )

squareToString : ( Int, Int ) -> String
squareToString (file, rank) =
    let f_name = case file of
            0 -> "A"
            1 -> "B"
            2 -> "C"
            3 -> "D"
            4 -> "E"
            5 -> "F"
            6 -> "G"
            7 -> "H"
            _ -> "NA"
    in String.append f_name <| String.fromInt <| 8 - rank

-- set bit for square
sqMask : Square -> UInt64.UInt64
sqMask sq = UInt64.shiftLeftBy sq UInt64.one 
