module Header exposing (renderHeader)

import Html exposing (Html, a, div, header, img, nav, text)
import Html.Attributes exposing (class, href, src, width)


renderHeader : Html msg
renderHeader =
    header [ class "header" ]
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
