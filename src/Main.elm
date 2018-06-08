module Main exposing (..)

import Html exposing (Html, a, aside, b, button, div, h1, header, img, nav, p, span, text)
import Html.Attributes exposing (alt, attribute, class, classList, disabled, href, id, src, type_, width)
import Html.Events exposing (onClick)
import Random exposing (generate, int, list)


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


isValid : List Ball -> Bool
isValid balls =
    countChecked balls /= 0


formatKenoLevel : Int -> String
formatKenoLevel kenoLevel =
    case kenoLevel of
        0 ->
            "Välj 1 - 11 nummer"

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


renderKenoLevel : String -> String -> Html Msg
renderKenoLevel level boardNumber =
    div [ class "keno-header" ]
        [ p [] [ text level, div [ class "board-number right" ] [ text boardNumber ] ]
        , span [] [ text "Kung Keno" ]
        ]


renderBoard : Board -> Html Msg
renderBoard board =
    let
        ballItems =
            List.map (renderBall board) board.balls
    in
    div [ class "board-container" ]
        [ renderKenoLevel (formatKenoLevel (countChecked board.balls)) (toString board.id)
        , div
            [ classList
                [ ( "board", True )
                , ( "isValid", isValid board.balls )
                ]
            ]
            ballItems
        , div [ class "board-buttons" ]
            [ button [ class "btn btn-300 btn-transparent-default resetBtn", disabled (isValid board.balls) ]
                [ text "Rensa" ]
            , button [ class "btn btn-300 btn-transparent-default huxfluxBtn", disabled True, onClick (Huxflux board) ]
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
                (List.map renderBoard model.boards)
            , aside
                [ class "win-plan" ]
                [ div
                    [ class "board-number" ]
                    [ text "1" ]
                , div
                    [ class "win-plan-table" ]
                    [ div
                        [ class "win-plan-table-header" ]
                        [ p [] [ text "Vinstplan" ], text "med radinsats 5kr" ]
                    ]
                , div [ class "win-plan-table-body" ]
                    [ span [] [ text "Antal rätt" ]
                    , span [ class "f-right" ] [ text "Vinst" ]
                    , span [ class "f-right" ] [ text "Vinst" ]
                    , span [] [ text "11" ]
                    , span [ class "f-right" ] [ text "-" ]
                    , span [ class "f-right" ] [ text "-" ]
                    , span [] [ text "10" ]
                    , span [ class "f-right" ] [ text "-" ]
                    , span [ class "f-right" ] [ text "-" ]
                    , span [] [ text "9" ]
                    , span [ class "f-right" ] [ text "-" ]
                    , span [ class "f-right" ] [ text "-" ]
                    , span [] [ text "8" ]
                    , span [ class "f-right" ] [ text "-" ]
                    , span [ class "f-right" ] [ text "-" ]
                    , span [] [ text "7" ]
                    , span [ class "f-right" ] [ text "-" ]
                    , span [ class "f-right" ] [ text "-" ]
                    , span [] [ text "6" ]
                    , span [ class "f-right" ] [ text "-" ]
                    , span [ class "f-right" ] [ text "-" ]
                    , span [] [ text "5" ]
                    , span [ class "f-right" ] [ text "-" ]
                    , span [ class "f-right" ] [ text "-" ]
                    , span [] [ text "4" ]
                    , span [ class "f-right" ] [ text "-" ]
                    , span [ class "f-right" ] [ text "-" ]
                    , span [] [ text "3" ]
                    , span [ class "f-right" ] [ text "-" ]
                    , span [ class "f-right" ] [ text "-" ]
                    , span [] [ text "2" ]
                    , span [ class "f-right" ] [ text "-" ]
                    , span [ class "f-right" ] [ text "-" ]
                    , span [] [ text "1" ]
                    , span [ class "f-right" ] [ text "-" ]
                    , span [ class "f-right" ] [ text "-" ]
                    ]
                , div [ class "win-plan-table-footer" ] [ b [] [ text "Kenofestival - " ], text "dubbla toppvinster på Keno 3, 4, 6, 8, 10, 11." ]
                ]
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
