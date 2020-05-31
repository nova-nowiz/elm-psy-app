module Session exposing (Session, changes, checkTokenExpiry, cred, fromCred, isExpired, navKey, role)

import Api exposing (Cred, Role)
import Browser.Navigation as Nav
import Debug exposing (log)
import Jwt exposing (JwtError)
import Task exposing (Task)
import Time exposing (Posix)



-- TYPES


type Session
    = LoggedIn Nav.Key Cred
    | Guest Nav.Key



-- INFO


cred : Session -> Maybe Cred
cred session =
    case session of
        LoggedIn _ val ->
            Just val

        Guest _ ->
            Nothing


navKey : Session -> Nav.Key
navKey session =
    case session of
        LoggedIn key _ ->
            key

        Guest key ->
            key


role : Session -> Role
role session =
    let
        credval =
            cred session
    in
    log "Role" Api.getRoleFromMaybeCred credval


checkTokenExpiry : Session -> Task Never JwtError
checkTokenExpiry session =
    let
        credval =
            cred session
    in
    Api.checkTokenExpiry credval


isExpired : Posix -> Session -> Result JwtError Bool
isExpired time session =
    let
        credval =
            cred session
    in
    Api.isExpired time credval



-- CHANGES


changes : (Session -> msg) -> Nav.Key -> Sub msg
changes toMsg key =
    Api.credChanges (\maybeCred -> toMsg (fromCred key maybeCred))


fromCred : Nav.Key -> Maybe Cred -> Session
fromCred key maybeCred =
    case maybeCred of
        Just credVal ->
            LoggedIn key credVal

        Nothing ->
            Guest key
