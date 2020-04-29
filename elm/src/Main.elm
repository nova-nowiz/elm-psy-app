module Main exposing (..)

import Api.Object.Agenda as APIAgenda
import Api.Object.Consultation as APIConsultation
import Api.Object.Patient as APIPatient
import Api.Object.Profession as APIProfession
import Api.Query as Query exposing (..)
import Api.Scalar exposing (..)
import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Graphql.Http exposing (HttpError(..))
import Graphql.Http.GraphqlError
import Graphql.Operation exposing (RootQuery)
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
    { graphqlData : GraphQLData }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Login
    , Cmd.none
    )



-- UPDATE


type Msg
    = GetPatients
    | GotResponse GraphQLData


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Login ->
            case msg of
                GetPatients ->
                    ( LoggedIn { graphqlData = Loading }, makeRequest )

                GotResponse _ ->
                    ( model, Cmd.none )

        LoggedIn data ->
            case msg of
                GetPatients ->
                    ( LoggedIn { data | graphqlData = Loading }, makeRequest )

                GotResponse graphQLData ->
                    case graphQLData of
                        NotAsked ->
                            ( model, Cmd.none )

                        Loading ->
                            ( model, Cmd.none )

                        Success response ->
                            ( LoggedIn { data | graphqlData = graphQLData }, Cmd.none )

                        Failure error ->
                            ( LoggedIn { data | graphqlData = graphQLData }, Cmd.none )



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
            case data.graphqlData of
                NotAsked ->
                    layout [] <|
                        el [ centerX, centerY ]
                            (text "The request was not made :/")

                Loading ->
                    layout [] <|
                        el [ centerX, centerY ]
                            (text "CHARGING MA LAYZOOOOOO")

                Success response ->
                    layout [] <|
                        column [ centerX, centerY, spacing 80 ]
                            (response
                                |> List.map
                                    patientRow
                            )

                Failure error ->
                    layout [] <|
                        el [ centerX, centerY ]
                            (error
                                |> errorToString
                                |> text
                            )


patientRow : Patient -> Element Msg
patientRow patient =
    row []
        [ tableField patient.prenom
        , tableField patient.nom
        , tableField <| String.fromInt patient.numero_rue
        , tableField patient.rue
        , tableField <| String.fromInt patient.code_postal
        , tableField patient.ville
        , tableField patient.pays
        , tableField
            (case patient.date_de_naissance of
                Date date ->
                    date
            )
        , tableField patient.genre
        , tableField patient.moyen_de_decouverte
        ]


tableField : String -> Element Msg
tableField data =
    el [ centerX, centerY, padding 25, Border.width 1 ] (text data)


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


type alias Response =
    List Patient


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


type alias GraphQLData =
    RemoteData (Graphql.Http.Error Response) Response


getPatients =
    Query.patient identity
        -- Patient is the type alias and thus the constructor of a record
        -- it will thus take all of these parameters as input
        (SelectionSet.succeed Patient
            |> with APIPatient.prenom
            |> with APIPatient.nom
            |> with APIPatient.numero_rue
            |> with APIPatient.rue
            |> with APIPatient.code_postal
            |> with APIPatient.ville
            |> with APIPatient.pays
            |> with APIPatient.date_de_naissance
            |> with APIPatient.genre
            |> with APIPatient.moyen_de_decouverte
        )


makeRequest : Cmd Msg
makeRequest =
    getPatients
        |> Graphql.Http.queryRequest "https://bdd-psy-app.herokuapp.com/v1/graphql"
        |> Graphql.Http.withHeader "x-hasura-admin-secret" "Dq4LwJ7PzeKTo4XYa6CoaqoQbPXtTZ9qEMHmgC46m78jTdVJvU"
        |> Graphql.Http.send (RemoteData.fromResult >> GotResponse)
