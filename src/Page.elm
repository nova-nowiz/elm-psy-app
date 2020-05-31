module Page exposing (Page(..), view)

import Api exposing (Cred)
import Browser exposing (Document)
import Element exposing (..)
import Html exposing (Html)
import Route exposing (Route)
import Session exposing (Session)


type Page
    = Other
    | Calendar
    | Patients


view : Maybe Cred -> Page -> { title : String, body : List (Element msg) } -> Document msg
view maybeCred page { title, body } =
    { title = title ++ " - PsyApp"
    , body = [ layout [] <| column [ width fill, height fill ] <| viewHeader page maybeCred :: body ]
    }


viewHeader : Page -> Maybe Cred -> Element msg
viewHeader page maybeCred =
    el [] (text "Todo: Here, there should be one or two buttons depending on if the user is a psy or not")
