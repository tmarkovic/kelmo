module Main exposing (..)

import Html exposing (Html, button, div, h1, img, text)
import Html.Attributes exposing (class, classList, src)
import Html.Events exposing (onClick)
import Random exposing (generate, list, int)


---- MODEL ----


type alias Ball =
    { number : Int, isChecked : Bool, isChosen : Bool }


type alias Board =
    { id : Int, balls : List Ball }


type alias Model =
    { boards : List Board }


model : Model
model =
    { boards =
        []
    }


init : ( Model, Cmd Msg )
init =
    ( { boards =
            List.range 1 6
                |> List.map
                    (\id ->
                        { id = id
                        , balls =
                            List.range 1 70
                                |> List.map (\x -> { number = x, isChecked = False, isChosen = False })
                        }
                    )
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
    = ToggleBall Bool Board Int
    | Huxflux Board
    | RandomBalls Board (List Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleBall chosen board number ->
            toggleBall chosen board number

        Huxflux board ->
            ( model, generate (RandomBalls board) (list 5 (int 0 100)) )

        RandomBalls board balls ->
            ( model, Cmd.none )


toggleBall : Bool -> Board -> Int -> ( Model, Cmd Msg )
toggleBall chosen board number =
    let
        updateBall b =
            if b.number == number then
                { b | isChecked = not b.isChecked, isChosen = chosen }
            else
                b

        updateBoard t =
            if t.id == board.id && (board.balls |> List.filter (\x -> x.isChecked) |> List.length) < 11 then
                { t | balls = List.map updateBall t.balls }
            else
                t
    in
        ( { model | boards = List.map updateBoard model.boards }, Cmd.none )



---- VIEW ----


renderBall : Board -> Ball -> Html Msg
renderBall board ball =
    div
        [ classList
            [ ( "ball", True )
            , ( "is-checked", ball.isChecked )
            ]
        , onClick (ToggleBall True board ball.number)
        ]
        [ toString ball.number
            |> text
        ]


renderKenoLevel : String -> Html Msg
renderKenoLevel level =
    h1 [ class "keno-header" ] [ text level ]


renderBoard : Board -> Html Msg
renderBoard board =
    let
        ballItems =
            List.map (renderBall board) board.balls
    in
        div []
            [ renderKenoLevel <| formatKenoLevel <| countChecked board.balls
            , div
                [ classList
                    [ ( "board", True )
                    , ( "isValid", board.balls |> List.any (\x -> x.isChecked) )
                    ]
                ]
                ballItems
            ]


view : Model -> Html Msg
view model =
    div [ class "boards" ]
        (List.map renderBoard model.boards)



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
