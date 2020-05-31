module Session exposing (Session, changes, cred, fromCred, navKey, role)

import Api exposing (Cred, Role)
import Browser.Navigation as Nav



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
    Api.getRoleFromMaybeCred credval



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
