module Main exposing (..)

import Api.InputObject exposing (..)
import Api.Mutation as Mutation exposing (..)
import Api.Object exposing (..)
import Api.Object.Patient
import Api.Object.Patient_mutation_response
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
    = Login
    | LoggedIn LoggedInModel


type alias LoggedInModel =
    { getPatientsData : GetPatientsData
    , addPatientData : AddPatientData
    , textInputs : TextInputs
    }


type alias TextInputs =
    { prenom : String
    , nom : String
    , numero_de_rue : String
    , rue : String
    , code_postal : String
    , ville : String
    , pays : String
    , date_de_naissance : String
    , genre : String
    , moyen_de_decouverte : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Login
    , Cmd.none
    )



-- UPDATE


type Msg
    = GetPatients
    | GetPatientsResponse GetPatientsData
    | AddPatientResponse AddPatientData
    | Type CurrentInput String
    | AddPatient


type CurrentInput
    = Prenom
    | Nom
    | Numero_de_rue
    | Rue
    | Code_postal
    | Ville
    | Pays
    | Date_de_naissance
    | Genre
    | Moyen_de_decouverte


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Login ->
            case msg of
                GetPatients ->
                    ( LoggedIn
                        { getPatientsData = Loading
                        , addPatientData = NotAsked
                        , textInputs =
                            { prenom = ""
                            , nom = ""
                            , numero_de_rue = ""
                            , rue = ""
                            , code_postal = ""
                            , ville = ""
                            , pays = ""
                            , date_de_naissance = ""
                            , genre = ""
                            , moyen_de_decouverte = ""
                            }
                        }
                    , getPatientsRequest
                    )

                GetPatientsResponse _ ->
                    ( model, Cmd.none )

                Type input text ->
                    ( model, Cmd.none )

                AddPatient ->
                    ( model, Cmd.none )

                AddPatientResponse addPatientData ->
                    ( model, Cmd.none )

        LoggedIn data ->
            case msg of
                GetPatients ->
                    ( LoggedIn { data | getPatientsData = Loading }, getPatientsRequest )

                GetPatientsResponse getPatientsData ->
                    case getPatientsData of
                        NotAsked ->
                            ( model, Cmd.none )

                        Loading ->
                            ( model, Cmd.none )

                        Success response ->
                            ( LoggedIn { data | getPatientsData = getPatientsData }, Cmd.none )

                        Failure error ->
                            ( LoggedIn { data | getPatientsData = getPatientsData }, Cmd.none )

                AddPatientResponse addPatientData ->
                    case addPatientData of
                        NotAsked ->
                            ( model, Cmd.none )

                        Loading ->
                            ( model, Cmd.none )

                        Success response ->
                            ( LoggedIn { data | addPatientData = addPatientData }, Cmd.none )

                        Failure error ->
                            ( LoggedIn { data | addPatientData = addPatientData }, Cmd.none )

                Type input text ->
                    textInputsUpdate data input text data.textInputs

                AddPatient ->
                    ( model, addPatientRequest data.textInputs )


textInputsUpdate : LoggedInModel -> CurrentInput -> String -> TextInputs -> ( Model, Cmd Msg )
textInputsUpdate data input text textInputs =
    case input of
        Prenom ->
            ( LoggedIn { data | textInputs = { textInputs | prenom = text } }, Cmd.none )

        Nom ->
            ( LoggedIn { data | textInputs = { textInputs | nom = text } }, Cmd.none )

        Numero_de_rue ->
            ( LoggedIn { data | textInputs = { textInputs | numero_de_rue = text } }, Cmd.none )

        Rue ->
            ( LoggedIn { data | textInputs = { textInputs | rue = text } }, Cmd.none )

        Code_postal ->
            ( LoggedIn { data | textInputs = { textInputs | code_postal = text } }, Cmd.none )

        Ville ->
            ( LoggedIn { data | textInputs = { textInputs | ville = text } }, Cmd.none )

        Pays ->
            ( LoggedIn { data | textInputs = { textInputs | pays = text } }, Cmd.none )

        Date_de_naissance ->
            ( LoggedIn { data | textInputs = { textInputs | date_de_naissance = text } }, Cmd.none )

        Genre ->
            ( LoggedIn { data | textInputs = { textInputs | genre = text } }, Cmd.none )

        Moyen_de_decouverte ->
            ( LoggedIn { data | textInputs = { textInputs | moyen_de_decouverte = text } }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        Login ->
            layout [] <|
                el [ centerX, centerY ]
                    (Input.button
                        []
                        { label = text "Request", onPress = Just GetPatients }
                    )

        LoggedIn data ->
            case data.getPatientsData of
                NotAsked ->
                    layout [] <|
                        el [ centerX, centerY ]
                            (text "The request was not made :/")

                Loading ->
                    layout [] <|
                        el [ centerX, centerY ]
                            (text "A proper expression")

                Success response ->
                    successView response data.textInputs

                Failure error ->
                    failureView error


successView : List Patient -> TextInputs -> Html Msg
successView response textInputs =
    layout [] <|
        column [ centerX, centerY ]
            [ patientTable response
            , row []
                [ textInput Prenom textInputs
                , textInput Nom textInputs
                , textInput Numero_de_rue textInputs
                , textInput Rue textInputs
                , textInput Code_postal textInputs
                , textInput Ville textInputs
                , textInput Pays textInputs
                , textInput Date_de_naissance textInputs
                , textInput Genre textInputs
                , textInput Moyen_de_decouverte textInputs
                ]
            , Input.button [ centerX, centerY ]
                { label =
                    el [ padding 30, Border.width 1, Border.rounded 5 ]
                        (text "Add a new Patient")
                , onPress = Just AddPatient
                }
            ]


textInput : CurrentInput -> TextInputs -> Element Msg
textInput currentInput textInputs =
    Input.text []
        { onChange = Type currentInput
        , text = currentInputString currentInput textInputs
        , placeholder = Just (Input.placeholder [] (text <| currentInputToString currentInput))
        , label = Input.labelAbove [] (text <| currentInputToString currentInput)
        }


currentInputString : CurrentInput -> TextInputs -> String
currentInputString currentInput textInputs =
    case currentInput of
        Prenom ->
            textInputs.prenom

        Nom ->
            textInputs.nom

        Numero_de_rue ->
            textInputs.numero_de_rue

        Rue ->
            textInputs.rue

        Code_postal ->
            textInputs.code_postal

        Ville ->
            textInputs.ville

        Pays ->
            textInputs.pays

        Date_de_naissance ->
            textInputs.date_de_naissance

        Genre ->
            textInputs.genre

        Moyen_de_decouverte ->
            textInputs.moyen_de_decouverte


currentInputToString : CurrentInput -> String
currentInputToString currentInput =
    case currentInput of
        Prenom ->
            "Prénom"

        Nom ->
            "Nom"

        Numero_de_rue ->
            "Numéro de rue"

        Rue ->
            "Rue"

        Code_postal ->
            "Code Postal"

        Ville ->
            "Ville"

        Pays ->
            "Pays"

        Date_de_naissance ->
            "Date de naissance"

        Genre ->
            "Genre"

        Moyen_de_decouverte ->
            "Moyen de découverte"


patientTable : List Patient -> Element Msg
patientTable response =
    table []
        { data = response
        , columns =
            [ { header = tableField "Prénom"
              , width = fill
              , view = \patient -> tableField patient.prenom
              }
            , { header = tableField "Nom"
              , width = fill
              , view = \patient -> tableField patient.nom
              }
            , { header = tableField "Numéro de rue"
              , width = fill
              , view = \patient -> tableField <| String.fromInt patient.numero_rue
              }
            , { header = tableField "Code postal"
              , width = fill
              , view = \patient -> tableField <| String.fromInt patient.code_postal
              }
            , { header = tableField "Ville"
              , width = fill
              , view = \patient -> tableField patient.ville
              }
            , { header = tableField "Pays"
              , width = fill
              , view = \patient -> tableField patient.pays
              }
            , { header = tableField "Date de naissance"
              , width = fill
              , view =
                    \patient ->
                        tableField
                            (case patient.date_de_naissance of
                                Date date ->
                                    date
                            )
              }
            , { header = tableField "Genre"
              , width = fill
              , view = \patient -> tableField patient.genre
              }
            , { header = tableField "Moyen de découverte"
              , width = fill
              , view = \patient -> tableField patient.moyen_de_decouverte
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


type alias Patient =
    { prenom : String
    , nom : String
    , numero_rue : Int
    , rue : String
    , code_postal : Int
    , ville : String
    , pays : String
    , date_de_naissance : Date
    , genre : String
    , moyen_de_decouverte : String
    }


type alias GetPatientsData =
    RemoteData (Graphql.Http.Error (List Patient)) (List Patient)


getPatient : SelectionSet Patient Api.Object.Patient
getPatient =
    -- Patient is the type alias and thus the constructor of a record
    -- it will thus take all of these parameters as input
    SelectionSet.succeed Patient
        |> with Api.Object.Patient.prenom
        |> with Api.Object.Patient.nom
        |> with Api.Object.Patient.numero_rue
        |> with Api.Object.Patient.rue
        |> with Api.Object.Patient.code_postal
        |> with Api.Object.Patient.ville
        |> with Api.Object.Patient.pays
        |> with Api.Object.Patient.date_de_naissance
        |> with Api.Object.Patient.genre
        |> with Api.Object.Patient.moyen_de_decouverte


getPatientsQuery : SelectionSet (List Patient) RootQuery
getPatientsQuery =
    Query.patient identity getPatient


getPatientsRequest : Cmd Msg
getPatientsRequest =
    getPatientsQuery
        |> Graphql.Http.queryRequest "https://bdd-psy-app.herokuapp.com/v1/graphql"
        |> Graphql.Http.withHeader "x-hasura-admin-secret" "Dq4LwJ7PzeKTo4XYa6CoaqoQbPXtTZ9qEMHmgC46m78jTdVJvU"
        |> Graphql.Http.send (RemoteData.fromResult >> GetPatientsResponse)


type alias AddPatientData =
    RemoteData (Graphql.Http.Error (Maybe (List Patient))) (Maybe (List Patient))


getPatientsMutation : SelectionSet (List Patient) Api.Object.Patient_mutation_response
getPatientsMutation =
    Api.Object.Patient_mutation_response.returning getPatient


addPatient : TextInputs -> SelectionSet (Maybe (List Patient)) RootMutation
addPatient textInputs =
    let
        patientinsert =
            { consultations = Absent
            , patient_Professions = Absent
            , code_postal = Present (String.toInt textInputs.code_postal |> Maybe.withDefault 0)
            , date_de_naissance = Present (Date textInputs.date_de_naissance)
            , genre = Present textInputs.genre
            , id_patient = Absent
            , moyen_de_decouverte = Present textInputs.moyen_de_decouverte
            , nom = Present textInputs.nom
            , numero_rue = Present (String.toInt textInputs.numero_de_rue |> Maybe.withDefault 0)
            , pays = Present textInputs.pays
            , prenom = Present textInputs.prenom
            , rue = Present textInputs.rue
            , ville = Present textInputs.ville
            }

        reqArgs : InsertPatientRequiredArguments
        reqArgs =
            InsertPatientRequiredArguments
                [ Patient_insert_input patientinsert ]
    in
    Mutation.insert_Patient (\optionals -> optionals)
        reqArgs
        getPatientsMutation



-- Patient is the type alias and thus the constructor of a record
-- it will thus take all of these parameters as input


addPatientRequest : TextInputs -> Cmd Msg
addPatientRequest textInputs =
    addPatient textInputs
        |> Graphql.Http.mutationRequest "https://bdd-psy-app.herokuapp.com/v1/graphql"
        |> Graphql.Http.withHeader "x-hasura-admin-secret" "Dq4LwJ7PzeKTo4XYa6CoaqoQbPXtTZ9qEMHmgC46m78jTdVJvU"
        |> Graphql.Http.send (RemoteData.fromResult >> AddPatientResponse)
