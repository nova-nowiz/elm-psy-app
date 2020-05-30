module Main exposing (..)

import Api.InputObject exposing (..)
import Api.Mutation as Mutation exposing (..)
import Api.Object exposing (..)
import Api.Object.Agenda
import Api.Object.Agenda_mutation_response
import Api.Query as Query exposing (..)
import Api.Scalar exposing (..)
import Api.ScalarCodecs
import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Graphql.Http exposing (HttpError(..))
import Graphql.Http.GraphqlError
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Html exposing (Html)
import Http
import Json.Decode exposing (Decoder, field, string)
import RemoteData exposing (RemoteData(..))



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type Model
    = Model RawModel


type alias RawModel =
    { getAgendaData : GetAgendaData
    , addAgendaData : AddAgendaData
    , form : Form
    }


type alias Form =
    { date : String
    , heure : String
    }


emptyForm : Form
emptyForm =
    { date = ""
    , heure = ""
    }


updateForm : (Form -> Form) -> RawModel -> ( Model, Cmd Msg )
updateForm transform data =
    ( Model { data | form = transform data.form }, Cmd.none )


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        { getAgendaData = Loading
        , addAgendaData = NotAsked
        , form = emptyForm
        }
    , getAgendaRequest
    )



-- UPDATE


type Msg
    = GetAgendaResponse GetAgendaData
    | AddAgendaResponse AddAgendaData
    | AddAgenda
      -- Form Inputs
    | EnteredDate String
    | EnteredHeure String



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        data =
            case model of
                Model datamodel ->
                    datamodel
    in
    case msg of
        GetAgendaResponse getAgendaData ->
            case getAgendaData of
                NotAsked ->
                    ( model, Cmd.none )

                Loading ->
                    ( model, Cmd.none )

                Success response ->
                    ( Model { data | getAgendaData = getAgendaData }, Cmd.none )

                Failure error ->
                    ( Model { data | getAgendaData = getAgendaData }, Cmd.none )

        AddAgendaResponse addAgendaData ->
            case addAgendaData of
                NotAsked ->
                    ( model, Cmd.none )

                Loading ->
                    ( model, Cmd.none )

                Success response ->
                    ( Model { data | addAgendaData = addAgendaData }, Cmd.none )

                Failure error ->
                    ( Model { data | addAgendaData = addAgendaData }, Cmd.none )

        AddAgenda ->
            ( model, addAgendaRequest data.form )

        EnteredDate text ->
            updateForm (\form -> { form | date = text }) data

        EnteredHeure text ->
            updateForm (\form -> { form | heure = text }) data



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    let
        data =
            case model of
                Model datamodel ->
                    datamodel
    in
    case data.getAgendaData of
        NotAsked ->
            layout [] <|
                el [ centerX, centerY ]
                    (text "The request was not made :/")

        Loading ->
            layout [] <|
                el [ centerX, centerY ]
                    (text "A proper expression")

        Success response ->
            successView response data.form

        Failure error ->
            failureView error


successView : List Agenda -> Form -> Html Msg
successView response form =
    layout [] <|
        column [ centerX, centerY, spacing 30 ]
            [ agendaTable response
            , row [ width fill ]
                [ textInput EnteredDate form.date "Prénom" "Prénom"
                , textInput EnteredHeure form.heure "Nom" "Nom"
                ]
            , Input.button [ centerX, centerY ]
                { label =
                    el [ padding 30, Border.width 1, Border.rounded 5 ]
                        (text "Add a new Agenda")
                , onPress = Just AddAgenda
                }
            ]


textInput : (String -> Msg) -> String -> String -> String -> Element Msg
textInput msg formtext placeholder label =
    Input.text []
        { onChange = msg
        , text = formtext
        , placeholder = Just (Input.placeholder [] (text placeholder))
        , label = Input.labelAbove [] (text label)
        }


agendaTable : List Agenda -> Element Msg
agendaTable response =
    table [ centerX, centerY ]
        { data = response
        , columns =
            [ { header = tableField "Date"
              , width = fill
              , view = \agenda -> tableField agenda.date
              }
            , { header = tableField "Heure"
              , width = fill
              , view = \agenda -> tableField agenda.heure
              }
            ]
        }


tableField : String -> Element Msg
tableField data =
    el [ centerX, centerY, padding 25, Border.width 1 ] (text data)


failureView : Graphql.Http.Error parsedData -> Html Msg
failureView error =
    layout [] <|
        el [ centerX, centerY ]
            (error
                |> errorToString
                |> text
            )


errorToString : Graphql.Http.Error parsedData -> String
errorToString errorData =
    case errorData of
        Graphql.Http.GraphqlError _ graphqlErrors ->
            graphqlErrors
                |> List.map graphqlErrorToString
                |> String.join "\n"

        Graphql.Http.HttpError httpError ->
            case httpError of
                BadUrl error ->
                    error

                Timeout ->
                    "There was a timeout"

                NetworkError ->
                    "There was a network error"

                BadStatus metadata error ->
                    error

                BadPayload jsonDecodeError ->
                    "There was a bad payload"


graphqlErrorToString : Graphql.Http.GraphqlError.GraphqlError -> String
graphqlErrorToString error =
    error.message



-- GRAPHQL


type alias Agenda =
    { date : String
    , heure : String
    }


type alias GetAgendaData =
    RemoteData (Graphql.Http.Error (List Agenda)) (List Agenda)


getAgenda : SelectionSet Agenda Api.Object.Agenda
getAgenda =
    -- Agenda is the type alias and thus the constructor of a record
    -- it will thus take all of these parameters as input
    SelectionSet.succeed Agenda
        |> with Api.Object.Agenda.date
        |> with Api.Object.Agenda.heure


getAgendaQuery : SelectionSet (List Agenda) RootQuery
getAgendaQuery =
    Query.agenda identity getAgenda


getAgendaRequest : Cmd Msg
getAgendaRequest =
    getAgendaQuery
        |> Graphql.Http.queryRequest "https://bdd-psy-app.herokuapp.com/v1/graphql"
        |> Graphql.Http.withHeader "x-hasura-admin-secret" "Dq4LwJ7PzeKTo4XYa6CoaqoQbPXtTZ9qEMHmgC46m78jTdVJvU"
        |> Graphql.Http.send (RemoteData.fromResult >> GetAgendaResponse)


type alias AddAgendaData =
    RemoteData (Graphql.Http.Error (Maybe (List Agenda))) (Maybe (List Agenda))


getAgendaMutation : SelectionSet (List Agenda) Api.Object.Agenda_mutation_response
getAgendaMutation =
    Api.Object.Agenda_mutation_response.returning getAgenda


addAgenda : Form -> SelectionSet (Maybe (List Agenda)) RootMutation
addAgenda form =
    let
        agendainsert =
            { consultations = Absent
            , date = Present form.date 
            , heure = Present form.heure
            , id_agenda = Absent
            }

        reqArgs : InsertAgendaRequiredArguments
        reqArgs =
            InsertAgendaRequiredArguments
                [ Agenda_insert_input agendainsert ]
    in
    Mutation.insert_Agenda (\optionals -> optionals)
        reqArgs
        getAgendaMutation



-- Agenda is the type alias and thus the constructor of a record
-- it will thus take all of these parameters as input


addAgendaRequest : Form -> Cmd Msg
addAgendaRequest form =
    addAgenda form
        |> Graphql.Http.mutationRequest "https://bdd-psy-app.herokuapp.com/v1/graphql"
        |> Graphql.Http.withHeader "x-hasura-admin-secret" "Dq4LwJ7PzeKTo4XYa6CoaqoQbPXtTZ9qEMHmgC46m78jTdVJvU"
        |> Graphql.Http.send (RemoteData.fromResult >> AddAgendaResponse)