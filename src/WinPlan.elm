module WinPlan exposing (..)

import Html exposing (Html, a, aside, b, div, header, img, nav, p, span, text)
import Html.Attributes exposing (class, href, src, width)


renderWinPlan : Html msg
renderWinPlan =
    aside
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
