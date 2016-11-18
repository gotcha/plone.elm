module Plone.Login exposing (..)

import Json.Encode exposing (object, string)
import Json.Decode as Json
import HttpBuilder
import Navigation
import Return exposing (Return, singleton, command)
import Task


-- MODEL


type alias Model =
    { connecting : Bool
    , user : Maybe User
    , baseUrl : String
    , form : Form
    }


type alias Form =
    { userid : String
    , password : String
    }


type alias User =
    { userid : String
    , token : String
    }


type alias Token =
    String


userid : Model -> String
userid model =
    case model.user of
        Just user ->
            user.userid

        Nothing ->
            "anonymous"


token : Model -> Token
token model =
    case model.user of
        Just user ->
            user.token

        Nothing ->
            ""


loginJson : Form -> Json.Encode.Value
loginJson form =
    object
        [ ( "login", string form.userid )
        , ( "password", string form.password )
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
            singleton { model | user = Nothing }

        LoginSucceed response ->
            singleton
                { model
                    | user = Just (User model.form.userid response.data)
                    , connecting = False
                }
                |> command (Navigation.newUrl "#home")

        LoginFail _ ->
            singleton { model | connecting = False }
                |> command (Navigation.newUrl "#home")

        LoginForm ->
            singleton { model | connecting = True }

        CancelLoginForm ->
            singleton
                { model
                    | user = Nothing
                    , connecting = False
                }
                |> command (Navigation.newUrl "#home")

        ChangePassword newPassword ->
            let
                form_ =
                    model.form

                form =
                    { form_ | password = newPassword }
            in
                singleton { model | form = form }

        ChangeUserId newUserid ->
            let
                form_ =
                    model.form

                form =
                    { form_ | userid = newUserid }
            in
                singleton { model | form = form }


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
            |> HttpBuilder.withJsonBody (loginJson model.form)
            |> HttpBuilder.send (HttpBuilder.jsonReader decodeToken) HttpBuilder.stringReader
