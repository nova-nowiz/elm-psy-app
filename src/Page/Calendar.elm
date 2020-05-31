module Page.Calendar exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Api.InputObject exposing (..)
import Api.Mutation as Mutation exposing (..)
import Api.Object exposing (..)
import Api.Object.Agenda
import Api.Object.Agenda_mutation_response
import Api.Query as Query exposing (..)
import Api.Scalar exposing (..)
import Api.ScalarCodecs
import Browser exposing (Document)
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
import Session exposing (Session)



-- MODEL


type Model
    = Model RawModel


type alias RawModel =
    { session : Session
    , getAgendaData : GetAgendaData
    , addAgendaData : AddAgendaData
    , deleteAgendaData : DeleteAgendaData
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


init : Session -> ( Model, Cmd Msg )
init session =
    ( Model
        { session = session
        , getAgendaData = Loading
        , addAgendaData = NotAsked
        , deleteAgendaData = NotAsked
        , form = emptyForm
        }
    , getAgendaRequest
    )



-- UPDATE


type Msg
    = GetAgendaResponse GetAgendaData
    | AddAgendaResponse AddAgendaData
    | DeleteAgendaResponse DeleteAgendaData
    | AddAgenda
    | DeleteAgenda Agenda
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
                    case response of
                        Just agenda ->
                            ( Model
                                { data
                                    | addAgendaData = addAgendaData
                                    , getAgendaData =
                                        case data.getAgendaData of
                                            Success agendadata ->
                                                Success (agendadata ++ agenda)

                                            meh ->
                                                meh
                                }
                            , Cmd.none
                            )

                        Nothing ->
                            ( Model { data | addAgendaData = addAgendaData }, Cmd.none )

                Failure error ->
                    ( Model { data | addAgendaData = addAgendaData }, Cmd.none )

        DeleteAgendaResponse deleteAgendaData ->
            case deleteAgendaData of
                NotAsked ->
                    ( model, Cmd.none )

                Loading ->
                    ( model, Cmd.none )

                Success response ->
                    case response of
                        Just agendas ->
                            ( Model
                                { data
                                    | deleteAgendaData = deleteAgendaData
                                    , getAgendaData =
                                        case data.getAgendaData of
                                            Success agendadata ->
                                                Success (List.filter (\agenda -> not (List.member agenda agendas)) agendadata)

                                            meh ->
                                                meh
                                }
                            , Cmd.none
                            )

                        Nothing ->
                            ( Model { data | deleteAgendaData = deleteAgendaData }, Cmd.none )

                Failure error ->
                    ( Model { data | deleteAgendaData = deleteAgendaData }, Cmd.none )

        AddAgenda ->
            ( model, addAgendaRequest data.form )

        DeleteAgenda agenda ->
            ( model, deleteAgendaRequest agenda )

        EnteredDate text ->
            updateForm (\form -> { form | date = text }) data

        EnteredHeure text ->
            updateForm (\form -> { form | heure = text }) data



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> { title : String, body : List (Element Msg) }
view model =
    { title = "Calendar"
    , body =
        [ let
            data =
                case model of
                    Model datamodel ->
                        datamodel
          in
          case data.getAgendaData of
            NotAsked ->
                el [ centerX, centerY ]
                    (text "La requête n'a pas abouti :/")

            Loading ->
                el [ centerX, centerY ]
                    (text "Nous importons vos données, merci de patienter.")

            Success response ->
                successView response data.form data.addAgendaData

            Failure error ->
                failureView error
        ]
    }


successView : List Agenda -> Form -> AddAgendaData -> Element Msg
successView response form addAgendaData =
    let
        error =
            case addAgendaData of
                Failure graphqlError ->
                    graphqlError
                        |> errorToString
                        |> text

                _ ->
                    text ""
    in
    column [ centerX, centerY, Background.color (rgb255 214 217 216), height fill, width fill ]
        [ row [ centerX, padding 50 ]
            [ image [ width (fill |> maximum 80) ] { src = "logo.png", description = "logo" }
            , el [ Font.color (rgb255 111 144 166), Font.size 80 ] (text "Votre liste de patients")
            , image [ width (fill |> maximum 80) ] { src = "logo.png", description = "logo" }
            ]
        , agendaTable response
        , row [ centerX, padding 30 ]
            [ textInput EnteredDate form.date "AAAA-MM-JJ" "Date"
            , textInput EnteredHeure form.heure "HH:MM:SS+TZ" "Heure"
            ]
        , Input.button [ centerX, centerY ]
            { label =
                el
                    [ padding 30
                    , Border.rounded 5
                    , Background.color (rgb255 111 144 166)
                    , mouseOver [ Background.color (rgb255 140 179 196) ]
                    , Element.focused [ Background.color (rgb255 24 52 61), Font.color (rgb255 214 217 216) ]
                    ]
                    (text "Ajouter un nouvel agenda")
            , onPress = Just AddAgenda
            }
        , el [ centerX, centerY, Font.color (rgb255 200 30 30) ]
            error
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
    let
        dateToString =
            \(Date string) -> string

        timeToString =
            \(Timetz string) -> string
    in
    table [ centerX, centerY, padding 30, Background.color (rgb255 111 144 166) ]
        { data = response
        , columns =
            [ { header = tableField "Date"
              , width = fill
              , view = \agenda -> tableField (dateToString agenda.date)
              }
            , { header = tableField "Heure"
              , width = fill
              , view = \agenda -> tableField (timeToString agenda.heure)
              }
            , { header = tableField "Supprimer agenda"
              , width = fill
              , view =
                    \agenda ->
                        Input.button [ Border.width 1, Background.color (rgb255 140 179 196) ]
                            --X background X
                            { onPress = Just (DeleteAgenda agenda)
                            , label =
                                el
                                    [ centerX
                                    , centerY
                                    , padding 23
                                    , Font.color (rgb255 255 50 50)
                                    , mouseOver [ Font.color (rgb255 200 30 30) ]
                                    , Element.focused [ Font.color (rgb255 100 10 10) ]
                                    ]
                                    (text "X")
                            }
              }
            ]
        }


tableField : String -> Element Msg
tableField data =
    el
        [ centerX
        , centerY
        , padding 23
        , Border.width 1
        , Background.color (rgb255 140 179 196)
        , Border.color (rgb255 24 52 61)
        ]
        (text data)


failureView : Graphql.Http.Error parsedData -> Element Msg
failureView error =
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
    { date : Date -- You have to use the types of graphql, else you get errors
    , heure : Timetz
    }


type alias GetAgendaData =
    RemoteData (Graphql.Http.Error (List Agenda)) (List Agenda)


getAgenda : SelectionSet Agenda Api.Object.Agenda
getAgenda =
    -- Agenda is the type alias and thus the constructor of a record
    -- it will thus take all of these parameters as input
    SelectionSet.succeed Agenda
        -- The Agenda constructor should take a Date and a Timetz
        -- as this is what we ask with the pipes
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
        -- Type annotations give more precise info when compiling
        agendainsert : Agenda_insert_inputRaw
        agendainsert =
            { consultations = Absent
            , date = Present (Date form.date) -- Date is a type, but it also is a constructor
            , heure = Present (Timetz form.heure)
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


type alias DeleteAgendaData =
    RemoteData (Graphql.Http.Error (Maybe (List Agenda))) (Maybe (List Agenda))


deleteAgenda : Agenda -> SelectionSet (Maybe (List Agenda)) RootMutation
deleteAgenda agenda =
    let
        reqArgs : DeleteAgendaRequiredArguments
        reqArgs =
            DeleteAgendaRequiredArguments
                (buildAgenda_bool_exp
                    (\record ->
                        { record
                            | date = Present (buildDate_comparison_exp (\r -> { r | eq_ = Present agenda.date }))
                            , heure = Present (buildTimetz_comparison_exp (\r -> { r | eq_ = Present agenda.heure }))
                        }
                    )
                )
    in
    Mutation.delete_Agenda
        reqArgs
        getAgendaMutation


deleteAgendaRequest : Agenda -> Cmd Msg
deleteAgendaRequest agenda =
    deleteAgenda agenda
        |> Graphql.Http.mutationRequest "https://bdd-psy-app.herokuapp.com/v1/graphql"
        |> Graphql.Http.withHeader "x-hasura-admin-secret" "Dq4LwJ7PzeKTo4XYa6CoaqoQbPXtTZ9qEMHmgC46m78jTdVJvU"
        |> Graphql.Http.send (RemoteData.fromResult >> DeleteAgendaResponse)



-- EXPORT


toSession : Model -> Session
toSession (Model rawModel) =
    rawModel.session
