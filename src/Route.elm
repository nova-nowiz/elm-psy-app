module Route exposing (Route(..), fromUrl, replaceUrl)

import Auth0.UrlParser exposing (Auth0CallbackInfo)
import Browser.Navigation as Nav
import Debug exposing (log)
import Html
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, string)



-- ROUTING


type Route
    = Root (Maybe Auth0CallbackInfo)
    | Calendar
    | Patients
    | Logout


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Root (Parser.fragment accessTokenUrlParser)
        , Parser.map Calendar (s "calendar")
        , Parser.map Patients (s "patients")
        , Parser.map Logout (s "logout")
        ]


accessTokenUrlParser : Maybe String -> Maybe Auth0CallbackInfo
accessTokenUrlParser maybeString =
    let
        fragment =
            maybeString |> Maybe.withDefault ""
    in
    if String.startsWith "access_token" (log "fragment" fragment) then
        String.split "&" fragment
            |> List.map (String.split "=")
            |> List.foldr
                (\item info ->
                    case item of
                        [ "access_token", token ] ->
                            { info | accessToken = token }

                        [ "id_token", token ] ->
                            { info | idToken = Just token }

                        [ "expires_in", sec ] ->
                            { info | expiresIn = String.toInt sec }

                        [ "token_type", tokenType ] ->
                            { info | tokenType = Just tokenType }

                        [ "state", state ] ->
                            { info | state = Just state }

                        _ ->
                            info
                )
                (Auth0CallbackInfo "" Nothing Nothing Nothing Nothing)
            |> Just

    else
        Nothing



-- PUBLIC HELPERS


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (routeToString route)


fromUrl : Url -> Maybe Route
fromUrl url =
    Parser.parse parser (log "url" url)



-- INTERNAL


routeToString : Route -> String
routeToString page =
    String.join "/" (routeToPieces page)


routeToPieces : Route -> List String
routeToPieces page =
    case page of
        Calendar ->
            [ "calendar" ]

        Patients ->
            [ "patients" ]

        _ ->
            []
