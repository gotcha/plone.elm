module Plone.Login exposing (..)

import Json.Encode exposing (object, string)
import Json.Decode as Json
import HttpBuilder
import Task


-- MODEL


type alias Security =
    { token : Maybe String
    , connecting : Bool
    , user : Maybe User
    , baseUrl : String
    }


type alias User =
    { userid : String
    , password : String
    }


userid : Security -> String
userid sec =
    case sec.user of
        Just user ->
            user.userid

        Nothing ->
            "anonymous"


password : Security -> String
password sec =
    case sec.user of
        Just user ->
            user.password

        Nothing ->
            ""


loginJson : String -> String -> Json.Encode.Value
loginJson userid password =
    object
        [ ( "login", string userid )
        , ( "password", string password )
        ]



-- UPDATE


type LoginMessage
    = LoginForm
    | CancelLoginForm
    | ChangePassword String
    | ChangeUserId String
    | LoggingIn
    | Logout
    | LoginSucceed (HttpBuilder.Response String)
    | LoginFail (HttpBuilder.Error String)


loginUpdate : LoginMessage -> Security -> ( Security, Cmd LoginMessage )
loginUpdate msg sec =
    case msg of
        LoggingIn ->
            ( sec, getToken sec )

        Logout ->
            ( { sec | token = Nothing }, Cmd.none )

        LoginSucceed response ->
            ( { sec
                | token = Just response.data
                , connecting = False
              }
            , Cmd.none
            )

        LoginFail _ ->
            ( { sec | connecting = False }, Cmd.none )

        LoginForm ->
            ( { sec | connecting = True }, Cmd.none )

        CancelLoginForm ->
            ( { sec
                | user = Nothing
                , connecting = False
              }
            , Cmd.none
            )

        ChangePassword newPassword ->
            let
                currentUserid =
                    userid sec

                user =
                    Just (User currentUserid newPassword)
            in
                ( { sec | user = user }, Cmd.none )

        ChangeUserId newUserid ->
            let
                currentPassword =
                    password sec

                user =
                    Just (User newUserid currentPassword)
            in
                ( { sec | user = user }, Cmd.none )


getToken : Security -> Cmd LoginMessage
getToken sec =
    Task.perform LoginFail
        LoginSucceed
        (fetchToken sec)


decodeToken : Json.Decoder String
decodeToken =
    Json.at [ "token" ] Json.string


fetchToken : Security -> Task.Task (HttpBuilder.Error String) (HttpBuilder.Response String)
fetchToken sec =
    let
        url =
            sec.baseUrl ++ "@login"
    in
        HttpBuilder.post url
            |> HttpBuilder.withHeaders
                [ ( "Accept", "application/json" )
                , ( "Content-Type", "application/json" )
                ]
            |> HttpBuilder.withJsonBody (loginJson (userid sec) (password sec))
            |> HttpBuilder.send (HttpBuilder.jsonReader decodeToken) HttpBuilder.stringReader
