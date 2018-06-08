module Main exposing (..)

import Html exposing (Html, a, button, div, h1, header, img, nav, text)
import Html.Attributes exposing (alt, attribute, class, classList, disabled, href, id, src, type_, width)
import Html.Events exposing (onClick)
import List exposing (filter, map, range, length, member, any, take)
import Random exposing (generate)
import Random.List exposing (shuffle)


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
            range 1 6
                |> map
                    (\id ->
                        { id = id
                        , balls =
                            range 1 70
                                |> map (\x -> { number = x, isChecked = False, isChosen = False })
                        }
                    )
      }
    , Cmd.none
    )



---- UTIL ----


countChecked : List Ball -> Int
countChecked balls =
    balls
        |> filter (\x -> x.isChecked == True)
        |> length


formatKenoLevel : Int -> String
formatKenoLevel kenoLevel =
    case kenoLevel of
        0 ->
            "Välj 1 - 11 nummer"

        _ ->
            "Keno " ++ toString kenoLevel



---- UPDATE ----


type Msg
    = ToggleBall Bool Board Int (List Int)
    | Huxflux Board
    | RandomBalls Board (List Int)


updateBalls : (Ball -> Ball) -> Int -> Board -> Board
updateBalls updateBall id t =
    if t.id == id then
        { t | balls = map updateBall t.balls }
    else
        t


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleBall chosen board check numbers ->
    let
                limitedNumbers =
                    take check numbers

                toggleBalls b =
                    if (not chosen || b.isChecked || (countChecked board.balls) < 11) && (member b.number limitedNumbers) then
                        { b | isChecked = not b.isChecked, isChosen = (not b.isChecked) && chosen }
            else
                b
            in
                ( { model | boards = map (updateBalls toggleBalls board.id) model.boards }, Cmd.none )

        Huxflux board ->
            ( { model
                | boards =
                    map
                        (updateBalls
                            (\x ->
                                if not x.isChosen then
                                    { x | isChecked = False }
            else
                                    x
                            )
                            board.id
                        )
                        model.boards
              }
            , generate (ToggleBall False board (11 - (board.balls |> (filter (\x -> x.isChosen)) |> length)))
                (board.balls
                    |> (filter (\x -> not x.isChosen))
                    |> (map (\x -> x.number))
                    |> shuffle
                )
            )



---- VIEW ----


renderBall : Board -> Ball -> Html Msg
renderBall board ball =
    div
        [ classList
            [ ( "ball", True )
            , ( "is-checked", ball.isChecked )
            ]
        , onClick (ToggleBall True board 1 [ ball.number ])
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
            map (renderBall board) board.balls
    in
    div [ class "board-container" ]
        [ renderKenoLevel <| formatKenoLevel <| countChecked board.balls
        , div
            [ classList
                [ ( "board", True )
                    , ( "isValid", board.balls |> any (\x -> x.isChecked) )
                ]
            ]
            ballItems
        , div [ class "board-buttons" ]
                [ button [ class "btn btn-300 btn-transparent-default resetBtn" ]
                [ text "Rensa" ]
                , button [ class "btn btn-300 btn-transparent-default huxfluxBtn", onClick (Huxflux board) ]
                [ text "HuxFlux" ]
            ]
        ]


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ header [ class "header" ]
            [ nav [ class "nav-container" ]
                [ div [ class "nav-items" ]
                    [ a [ class "nav-item", href "" ]
                        [ text "Start" ]
                    , a [ class "nav-item active", href "" ]
                        [ text "Spela" ]
                    , a [ class "nav-item", href "" ]
                        [ text "Arenan" ]
                    , a [ class "nav-item", href "" ]
                        [ text "Resultat" ]
                    , a [ class "nav-item", href "" ]
                        [ text "Sportservice" ]
                    , a [ class "nav-item", href "" ]
                        [ text "Mer" ]
                    , a [ class "nav-item", href "" ]
                        [ text "Hjälp" ]
                    , a [ class "nav-item", href "" ]
                        [ text "Mina Spel" ]
                    , a [ class "nav-item", href "" ]
                        [ text "Bli Kund" ]
                    , a [ class "nav-item", href "" ]
                        [ text "Logga in" ]
                    ]
                ]
            , div [ class "header-container" ]
                [ img [ width 220, src "https://om.svenskaspel.se/AnnualReport/2016/globalassets/spelloggor/svs_keno.png" ] [] ]
            , div [ class "game-info" ]
                [ text "Spelstopp: 18:25" ]
            ]
        , div [ class "content" ]
            [ div
                [ class "boards" ]
                (map renderBoard model.boards)
            ]
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
