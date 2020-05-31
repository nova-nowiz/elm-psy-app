module Main exposing (main)

import Api exposing (Cred)
import Browser exposing (Document)
import Browser.Navigation as Nav
import Debug exposing (log)
import Element exposing (..)
import Element.Font as Font
import Html exposing (Html)
import Json.Decode as Decode exposing (Value)
import Jwt exposing (JwtError)
import Page exposing (Page)
import Page.Calendar as Calendar
import Page.Patients as Patients
import Route exposing (Route)
import Session exposing (Session)
import Task
import Time exposing (Posix)
import Url exposing (Url)


type Model
    = Redirect Session
    | NotFound Session
    | Calendar Calendar.Model
    | Patients Patients.Model



-- MODEL


init : Maybe Cred -> Url -> Nav.Key -> ( Model, Cmd Msg )
init maybeCred url navKey =
    let
        session =
            Session.fromCred navKey maybeCred
    in
    changeRouteTo (Route.fromUrl url)
        (Redirect session)



-- VIEW


view : Model -> Document Msg
view model =
    let
        cred =
            Session.cred (toSession model)

        viewPage page toMsg config =
            let
                { title, body } =
                    Page.view cred page config
            in
            { title = title
            , body = List.map (Html.map toMsg) body
            }
    in
    case model of
        Redirect _ ->
            Page.view cred Page.Other { title = "", body = [ text "" ] }

        NotFound _ ->
            Page.view cred Page.Other { title = "", body = [ el [ centerX, centerY, Font.size 30 ] (text "Page not found") ] }

        Calendar calendar ->
            viewPage Page.Calendar GotCalendarMsg (Calendar.view calendar)

        Patients patients ->
            viewPage Page.Patients GotPatientsMsg (Patients.view patients)



-- UPDATE


type Msg
    = ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GotCalendarMsg Calendar.Msg
    | GotPatientsMsg Patients.Msg
    | GotSession Session
    | CheckToken Posix


toSession : Model -> Session
toSession page =
    case page of
        Redirect session ->
            session

        NotFound session ->
            session

        Calendar calendar ->
            Calendar.toSession calendar

        Patients patients ->
            Patients.toSession patients


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session =
            toSession model

        cred =
            Session.cred session
    in
    case cred of
        Just _ ->
            case log "maybeRoute (l112)" maybeRoute of
                Nothing ->
                    ( NotFound session, Cmd.none )

                Just Route.Calendar ->
                    Calendar.init session
                        |> updateWith Calendar GotCalendarMsg

                Just Route.Patients ->
                    Patients.init session
                        |> updateWith Patients GotPatientsMsg

                Just _ ->
                    -- Redirects root to calendar page,
                    -- I could have deleted it and in route.elm I could
                    -- have put the Parser.top as a Route.Calendar
                    -- but this is way clearer
                    ( Redirect session, Cmd.batch [ Route.replaceUrl (Session.navKey session) Route.Calendar, Task.perform CheckToken Time.now ] )

        Nothing ->
            case maybeRoute of
                Just (Route.Root maybeCallbackInfo) ->
                    case maybeCallbackInfo of
                        Just callbackInfo ->
                            ( Redirect session, Api.storeCredFromAuth callbackInfo )

                        Nothing ->
                            -- Redirects to login page if the expression is malformed
                            ( Redirect session, Nav.load "https://psy-app.eu.auth0.com/login?client=rcd2TG98zW4rEN4mq3PgxEe3hMQfPDWf&protocol=oauth2&response_type=token%20id_token&redirect_uri=http://localhost:8000&scope=openid%20profile" )

                _ ->
                    -- Redirects to login page if there are no credentials
                    ( Redirect session, Nav.load "https://psy-app.eu.auth0.com/login?client=rcd2TG98zW4rEN4mq3PgxEe3hMQfPDWf&protocol=oauth2&response_type=token%20id_token&redirect_uri=http://localhost:8000&scope=openid%20profile" )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl (Session.navKey (toSession model)) (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        ( ChangedUrl url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( GotCalendarMsg subMsg, Calendar calendar ) ->
            Calendar.update subMsg calendar
                |> updateWith Calendar GotCalendarMsg

        ( GotPatientsMsg subMsg, Patients patients ) ->
            Patients.update subMsg patients
                |> updateWith Patients GotPatientsMsg

        ( GotSession session, Redirect _ ) ->
            ( Redirect session
            , Route.replaceUrl (Session.navKey session) Route.Calendar
            )

        ( CheckToken time, _ ) ->
            if log "isExpired" (checkToken time model) then
                -- If token is expired then we logout the user
                ( Redirect <| toSession model, Api.logout )

            else
                ( model, Cmd.none )

        ( _, _ ) ->
            -- Disregard messages that arrived for the wrong page.
            ( model, Cmd.none )


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.batch [ Cmd.map toMsg subCmd, Task.perform CheckToken Time.now ]
    )


checkToken : Posix -> Model -> Bool
checkToken time model =
    Result.withDefault False <| Session.isExpired time <| toSession model



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (5 * 60 * 1000) CheckToken
        , case model of
            NotFound _ ->
                Sub.none

            Redirect _ ->
                Session.changes GotSession (Session.navKey (toSession model))

            Calendar calendar ->
                Sub.map GotCalendarMsg (Calendar.subscriptions calendar)

            Patients patients ->
                Sub.map GotPatientsMsg (Patients.subscriptions patients)
        ]



-- MAIN


main : Program Value Model Msg
main =
    Api.application
        { init = init
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
