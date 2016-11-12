module Plone.Login exposing (..)

import Json.Encode exposing (object, string)
import Json.Decode as Json
import HttpBuilder
import Return exposing (Return, singleton, command)
import Task


-- MODEL


type alias Model =
    { token : Maybe String
    , connecting : Bool
    , user : Maybe User
    , baseUrl : String
    }


type alias User =
    { userid : String
    , password : String
    }


userid : Model -> String
userid model =
    case model.user of
        Just user ->
            user.userid

        Nothing ->
            "anonymous"


password : Model -> String
password model =
    case model.user of
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


type Msg
    = LoginForm
    | CancelLoginForm
    | ChangePassword String
    | ChangeUserId String
    | GetToken
    | Logout
    | LoginSucceed (HttpBuilder.Response String)
    | LoginFail (HttpBuilder.Error String)


update : Msg -> Model -> Return Msg Model
update msg model =
    case msg of
        GetToken ->
            singleton model |> command (getTokenCmd model)

        Logout ->
            singleton { model | token = Nothing }

        LoginSucceed response ->
            singleton
                { model
                    | token = Just response.data
                    , connecting = False
                }

        LoginFail _ ->
            singleton { model | connecting = False }

        LoginForm ->
            singleton { model | connecting = True }

        CancelLoginForm ->
            singleton
                { model
                    | user = Nothing
                    , connecting = False
                }

        ChangePassword newPassword ->
            let
                currentUserid =
                    userid model

                user =
                    Just (User currentUserid newPassword)
            in
                singleton { model | user = user }

        ChangeUserId newUserid ->
            let
                currentPassword =
                    password model

                user =
                    Just (User newUserid currentPassword)
            in
                singleton { model | user = user }


getTokenCmd : Model -> Cmd Msg
getTokenCmd model =
    Task.perform LoginFail
        LoginSucceed
        (getTokenRequest model)


decodeToken : Json.Decoder String
decodeToken =
    Json.at [ "token" ] Json.string


getTokenRequest : Model -> Task.Task (HttpBuilder.Error String) (HttpBuilder.Response String)
getTokenRequest model =
    let
        url =
            model.baseUrl ++ "@login"
    in
        HttpBuilder.post url
            |> HttpBuilder.withHeaders
                [ ( "Accept", "application/json" )
                , ( "Content-Type", "application/json" )
                ]
            |> HttpBuilder.withJsonBody (loginJson (userid model) (password model))
            |> HttpBuilder.send (HttpBuilder.jsonReader decodeToken) HttpBuilder.stringReader
