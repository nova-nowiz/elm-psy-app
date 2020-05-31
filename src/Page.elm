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


view : Maybe Cred -> Page -> Document msg -> Document msg
view maybeCred page { title, body } =
    { title = title ++ " - PsyApp"
    , body = viewHeader page maybeCred :: body
    }


viewHeader : Page -> Maybe Cred -> Html msg
viewHeader page maybeCred =
    layout [] <| el [] (text "Todo: Here, there should be one or two buttons depending on if the user is a psy or not")
