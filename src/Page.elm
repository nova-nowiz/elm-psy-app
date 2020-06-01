module Page exposing (view)

import Api exposing (Cred, Role)
import Browser exposing (Document)
import Debug exposing (log)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html exposing (Html)
import Route exposing (Route)
import Session exposing (Session)


view : Maybe Cred -> { title : String, body : List (Element msg) } -> Document msg
view maybeCred { title, body } =
    { title = title ++ " - PsyApp"
    , body = [ layout [] <| column [ width fill, height fill ] <| viewHeader maybeCred :: body ]
    }


viewHeader : Maybe Cred -> Element msg
viewHeader maybeCred =
    row [ width fill, Background.color (rgb255 111 144 166) ] <|
        (case log "Role" (Api.getRoleFromMaybeCred maybeCred) of
            Api.Psy ->
                [ link [ width fill ]
                    { url = "patients"
                    , label =
                        el
                            [ width fill
                            , padding 30
                            , Background.color (rgb255 111 144 166)
                            , mouseOver [ Background.color (rgb255 140 179 196) ]
                            , Element.focused [ Background.color (rgb255 24 52 61), Font.color (rgb255 214 217 216) ]
                            ]
                        <|
                            el [ centerX, centerY ] (text "Patients")
                    }
                , link [ width fill ]
                    { url = "calendar"
                    , label =
                        el
                            [ width fill
                            , padding 30
                            , Background.color (rgb255 111 144 166)
                            , mouseOver [ Background.color (rgb255 140 179 196) ]
                            , Element.focused [ Background.color (rgb255 24 52 61), Font.color (rgb255 214 217 216) ]
                            ]
                        <|
                            el [ centerX, centerY ] (text "Calendrier")
                    }
                ]

            _ ->
                [ none ]
        )
            ++ [ link [ width fill ]
                    { url = "logout"
                    , label =
                        el
                            [ width fill
                            , padding 30
                            , Background.color (rgb255 111 144 166)
                            , mouseOver [ Background.color (rgb255 140 179 196) ]
                            , Element.focused [ Background.color (rgb255 24 52 61), Font.color (rgb255 214 217 216) ]
                            ]
                        <|
                            el [ centerX, centerY ] (text "Logout")
                    }
               ]
