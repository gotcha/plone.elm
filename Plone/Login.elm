module Plone.Login exposing (..)

import Json.Encode exposing (object, string)
import Json.Decode as Json
import HttpBuilder
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
    | LoggingIn
    | Logout
    | LoginSucceed (HttpBuilder.Response String)
    | LoginFail (HttpBuilder.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoggingIn ->
            ( model, getToken model )

        Logout ->
            ( { model | token = Nothing }, Cmd.none )

        LoginSucceed response ->
            ( { model
                | token = Just response.data
                , connecting = False
              }
            , Cmd.none
            )

        LoginFail _ ->
            ( { model | connecting = False }, Cmd.none )

        LoginForm ->
            ( { model | connecting = True }, Cmd.none )

        CancelLoginForm ->
            ( { model
                | user = Nothing
                , connecting = False
              }
            , Cmd.none
            )

        ChangePassword newPassword ->
            let
                currentUserid =
                    userid model

                user =
                    Just (User currentUserid newPassword)
            in
                ( { model | user = user }, Cmd.none )

        ChangeUserId newUserid ->
            let
                currentPassword =
                    password model

                user =
                    Just (User newUserid currentPassword)
            in
                ( { model | user = user }, Cmd.none )


getToken : Model -> Cmd Msg
getToken model =
    Task.perform LoginFail
        LoginSucceed
        (fetchToken model)


decodeToken : Json.Decoder String
decodeToken =
    Json.at [ "token" ] Json.string


fetchToken : Model -> Task.Task (HttpBuilder.Error String) (HttpBuilder.Response String)
fetchToken model =
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
