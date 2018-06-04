module Tests exposing (..)

import Expect
import Main exposing (..)
import Test exposing (..)


-- Check out http://package.elm-lang.org/packages/elm-community/elm-test/latest to learn more about testing in Elm!


all : Test
all =
    describe "Utils"
        [ test "Format Keno level 0" <|
            \_ ->
                Expect.equal "Keno" (formatKenoLevel 0)
        , test "Format Keno level 1" <|
            \_ ->
                Expect.equal "Keno 1" (formatKenoLevel 1)
        , test "Count checked 1" <|
            \_ ->
                Expect.equal 1 (countChecked [ { number = 1, isChecked = False }, { number = 1, isChecked = True } ])
        , test "Count checked 0" <|
            \_ ->
                Expect.equal 0 (countChecked [ { number = 1, isChecked = False } ])
        ]
