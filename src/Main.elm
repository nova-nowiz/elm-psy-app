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
    = Model RawModel


type alias RawModel =
    { getPatientsData : GetPatientsData
    , addPatientData : AddPatientData
    , form : Form
    }


type alias Form =
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


emptyForm : Form
emptyForm =
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


updateForm : (Form -> Form) -> RawModel -> ( Model, Cmd Msg )
updateForm transform data =
    ( Model { data | form = transform data.form }, Cmd.none )


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        { getPatientsData = Loading
        , addPatientData = NotAsked
        , form = emptyForm
        }
    , getPatientsRequest
    )



-- UPDATE


type Msg
    = GetPatientsResponse GetPatientsData
    | AddPatientResponse AddPatientData
    | AddPatient
      -- Form Inputs
    | EnteredPrenom String
    | EnteredNom String
    | EnteredNumero_de_rue String
    | EnteredRue String
    | EnteredCode_postal String
    | EnteredVille String
    | EnteredPays String
    | EnteredDate_de_naissance String
    | EnteredGenre String
    | EnteredMoyen_de_decouverte String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        data =
            case model of
                Model datamodel ->
                    datamodel
    in
    case msg of
        GetPatientsResponse getPatientsData ->
            case getPatientsData of
                NotAsked ->
                    ( model, Cmd.none )

                Loading ->
                    ( model, Cmd.none )

                Success response ->
                    ( Model { data | getPatientsData = getPatientsData }, Cmd.none )

                Failure error ->
                    ( Model { data | getPatientsData = getPatientsData }, Cmd.none )

        AddPatientResponse addPatientData ->
            case addPatientData of
                NotAsked ->
                    ( model, Cmd.none )

                Loading ->
                    ( model, Cmd.none )

                Success response ->
                    case response of
                        Just patients ->
                            ( Model { data | addPatientData = addPatientData, getPatientsData = Success patients }, Cmd.none )

                        Nothing ->
                            ( Model { data | addPatientData = addPatientData }, Cmd.none )

                Failure error ->
                    ( Model { data | addPatientData = addPatientData }, Cmd.none )

        AddPatient ->
            ( model, addPatientRequest data.form )

        EnteredPrenom text ->
            updateForm (\form -> { form | prenom = text }) data

        EnteredNom text ->
            updateForm (\form -> { form | nom = text }) data

        EnteredNumero_de_rue text ->
            updateForm (\form -> { form | numero_de_rue = text }) data

        EnteredRue text ->
            updateForm (\form -> { form | rue = text }) data

        EnteredCode_postal text ->
            updateForm (\form -> { form | code_postal = text }) data

        EnteredVille text ->
            updateForm (\form -> { form | ville = text }) data

        EnteredPays text ->
            updateForm (\form -> { form | pays = text }) data

        EnteredDate_de_naissance text ->
            updateForm (\form -> { form | date_de_naissance = text }) data

        EnteredGenre text ->
            updateForm (\form -> { form | genre = text }) data

        EnteredMoyen_de_decouverte text ->
            updateForm (\form -> { form | moyen_de_decouverte = text }) data



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
            successView response data.form data.addPatientData

        Failure error ->
            failureView error


successView : List Patient -> Form -> AddPatientData -> Html Msg
successView response form addPatientData =
    let
        error =
            case addPatientData of
                Failure graphqlError ->
                    graphqlError
                        |> errorToString
                        |> text

                _ ->
                    text ""
    in
    layout [] <|
        column [ centerX, centerY, spacing 30 ]
            [ patientTable response
            , row [ width fill ]
                [ textInput EnteredPrenom form.prenom "Prénom" "Prénom"
                , textInput EnteredNom form.nom "Nom" "Nom"
                , textInput EnteredNumero_de_rue form.numero_de_rue "Numéro de rue" "Numéro de rue"
                , textInput EnteredRue form.rue "Rue" "Rue"
                , textInput EnteredCode_postal form.code_postal "Code postal" "Code postal"
                , textInput EnteredVille form.ville "Ville" "Ville"
                , textInput EnteredPays form.pays "Pays" "Pays"
                , textInput EnteredDate_de_naissance form.date_de_naissance "Date de naissance" "Date de naissance"
                , textInput EnteredGenre form.genre "Genre" "Genre"
                , textInput EnteredMoyen_de_decouverte form.moyen_de_decouverte "Moyen de découverte" "Moyen de découverte"
                ]
            , Input.button [ centerX, centerY ]
                { label =
                    el [ padding 30, Border.width 1, Border.rounded 5 ]
                        (text "Add a new Patient")
                , onPress = Just AddPatient
                }
            , el [ centerX, centerY ]
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


patientTable : List Patient -> Element Msg
patientTable response =
    table [ centerX, centerY ]
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
            , { header = tableField "Rue"
              , width = fill
              , view = \patient -> tableField patient.rue
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


addPatient : Form -> SelectionSet (Maybe (List Patient)) RootMutation
addPatient form =
    let
        patientinsert =
            { consultations = Absent
            , patient_Professions = Absent
            , code_postal = Present (String.toInt form.code_postal |> Maybe.withDefault 0)
            , date_de_naissance = Present (Date form.date_de_naissance)
            , genre = Present form.genre
            , id_patient = Absent
            , moyen_de_decouverte = Present form.moyen_de_decouverte
            , nom = Present form.nom
            , numero_rue = Present (String.toInt form.numero_de_rue |> Maybe.withDefault 0)
            , pays = Present form.pays
            , prenom = Present form.prenom
            , rue = Present form.rue
            , ville = Present form.ville
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


addPatientRequest : Form -> Cmd Msg
addPatientRequest form =
    addPatient form
        |> Graphql.Http.mutationRequest "https://bdd-psy-app.herokuapp.com/v1/graphql"
        |> Graphql.Http.withHeader "x-hasura-admin-secret" "Dq4LwJ7PzeKTo4XYa6CoaqoQbPXtTZ9qEMHmgC46m78jTdVJvU"
        |> Graphql.Http.send (RemoteData.fromResult >> AddPatientResponse)
