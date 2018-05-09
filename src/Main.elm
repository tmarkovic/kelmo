module Main exposing (..)

import Html exposing (Html, button, div, h1, img, text)
import Html.Attributes exposing (class, classList, src)
import Html.Events exposing (onClick)


---- MODEL ----


type alias Ball =
    { number : Int, isChecked : Bool }


type alias Model =
    { balls : List Ball, isValid : Bool }


model : Model
model =
    { balls = []
    , isValid = False
    }


init : ( Model, Cmd Msg )
init =
    ( { balls =
            List.range 1 70
                |> List.map (\x -> { number = x, isChecked = False })
      , isValid = False
      }
    , Cmd.none
    )



---- UTIL ----


countChecked : List Ball -> Int
countChecked balls =
    balls
        |> List.filter (\x -> x.isChecked == True)
        |> List.length


formatKenoLevel : Int -> String
formatKenoLevel kenoLevel =
    case kenoLevel of
        0 ->
            "Keno"

        _ ->
            "Keno " ++ toString kenoLevel



---- UPDATE ----


type Msg
    = ToggleBall Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleBall number ->
            let
                updateEntry t =
                    if t.number == number then
                        { t | isChecked = not t.isChecked }
                    else
                        t
            in
            { model | balls = List.map updateEntry model.balls }
                ! []



---- VIEW ----


renderBall : Ball -> Html Msg
renderBall model =
    div
        [ classList
            [ ( "result-ball", True )
            , ( "is-checked", model.isChecked )
            ]
        , onClick (ToggleBall model.number)
        ]
        [ toString model.number
            |> text
        ]


renderKenoLevel : String -> Html Msg
renderKenoLevel level =
    h1 [ class "keno-header" ] [ text level ]


renderBoard : List Ball -> Html Msg
renderBoard balls =
    let
        ballItems =
            List.map renderBall balls
    in
    div []
        [ renderKenoLevel <| formatKenoLevel <| countChecked balls
        , div [ class "board" ] ballItems
        ]


view : Model -> Html Msg
view model =
    div [ class "board-container" ]
        [ renderBoard model.balls
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
