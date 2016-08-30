module Main exposing (..)

import Html
    exposing
        ( div
        , text
        , button
        , hr
        , h2
        , input
        , pre
        )
import Html.App
import Html.Events as Events
import Html.Attributes as Attr
import Task
import Http
import Json.Decode as Json
import Json.Encode exposing (encode, object, string)
import Debug


main =
    Html.App.program
        { init = init "no title"
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { title : String
    , user : Maybe User
    , logging : Bool
    , token : Maybe String
    }


type alias User =
    { userid : String
    , password : String
    }


init : String -> ( Model, Cmd Msg )
init title =
    ( Model title Nothing False Nothing
    , Cmd.none
    )


isLoggedIn : Model -> Bool
isLoggedIn model =
    case model.token of
        Just token ->
            True

        Nothing ->
            False


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


login userid password =
    object
        [ ( "login", string userid )
        , ( "password", string password )
        ]


loginJSON model =
    encode 4 (login (userid model) (password model))


titleJSON model =
    encode 4 (object [ ( "title", string model.title ) ])



-- UPDATE


type Msg
    = Fetch
    | FetchSucceed String
    | FetchFail Http.Error
    | LoginForm
    | CancelLoginForm
    | ChangePassword String
    | ChangeUserId String
    | ChangeTitle String
    | UpdateTitle
    | LoggingIn
    | LoginSucceed String
    | LoginFail Http.Error
    | UpdateSucceed String
    | UpdateFail Http.Error


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "model" msg of
        Fetch ->
            ( model, getDocumentTitle )

        FetchSucceed newTitle ->
            ( { model | title = newTitle }, Cmd.none )

        FetchFail _ ->
            ( model, Cmd.none )

        LoggingIn ->
            ( model, getToken model )

        UpdateTitle ->
            ( model, updateTitle model )

        LoginSucceed newToken ->
            ( { model
                | token = Just newToken
                , logging = False
              }
            , Cmd.none
            )

        UpdateFail _ ->
            ( model, Cmd.none )

        UpdateSucceed _ ->
            ( model, Cmd.none )

        LoginFail _ ->
            ( { model | logging = False }, Cmd.none )

        LoginForm ->
            ( { model | logging = True }, Cmd.none )

        CancelLoginForm ->
            ( { model | logging = False, user = Nothing }, Cmd.none )

        ChangePassword newPassword ->
            let
                currentUserid =
                    userid model
            in
                ( { model | user = Just (User currentUserid newPassword) }
                , Cmd.none
                )

        ChangeUserId newUserid ->
            let
                currentPassword =
                    password model
            in
                ( { model | user = Just (User newUserid currentPassword) }
                , Cmd.none
                )

        ChangeTitle newTitle ->
            ( { model | title = newTitle }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html.Html Msg
view model =
    if model.logging then
        loginFormView model
    else
        mainView model


loginFormView model =
    div []
        [ h2 [] [ text "Login Form" ]
        , input [ Attr.placeholder "Userid", Events.onInput ChangeUserId ] []
        , input [ Attr.placeholder "Password", Events.onInput ChangePassword ] []
        , button [ Events.onClick LoggingIn ] [ text "Login" ]
        , button [ Events.onClick CancelLoginForm ] [ text "Cancel" ]
        , debugView model
        ]


mainView model =
    div []
        [ loginView model
        , titleView model
        , debugView model
        ]


titleView model =
    if isLoggedIn model then
        updateTitleView model
    else
        displayTitleView model


displayTitleView model =
    div []
        [ button [ Events.onClick Fetch ]
            [ text "Refresh" ]
        , div [] [ text model.title ]
        ]


updateTitleView model =
    div []
        [ input [ Attr.placeholder model.title, Events.onInput ChangeTitle ] []
        , button [ Events.onClick UpdateTitle ]
            [ text "Update" ]
        , div [] [ text model.title ]
        ]


debugView model =
    div []
        [ hr [] []
        , text (toString model)
        , hr [] []
        , pre [] [ text (loginJSON model) ]
        ]


loginView model =
    if isLoggedIn model then
        div [] [ text (userid model) ]
    else
        button [ Events.onClick LoginForm ] [ text "Login" ]



-- HTTP


getDocumentTitle : Cmd Msg
getDocumentTitle =
    let
        url =
            "http://localhost:8080/Plone/front-page"
    in
        Task.perform FetchFail
            FetchSucceed
            (Http.fromJson decodeTitle (ploneGet url))


ploneGet : String -> Task.Task Http.RawError Http.Response
ploneGet url =
    Http.send Http.defaultSettings
        { verb = "GET"
        , headers = [ ( "Accept", "application/json" ) ]
        , url = url
        , body = Http.empty
        }


getToken : Model -> Cmd Msg
getToken model =
    let
        url =
            "http://localhost:8080/Plone/@login"
    in
        Task.perform LoginFail
            LoginSucceed
            (Http.fromJson decodeToken (ploneAuth url model))


ploneAuth : String -> Model -> Task.Task Http.RawError Http.Response
ploneAuth url model =
    Http.send Http.defaultSettings
        { verb = "POST"
        , headers =
            [ ( "Accept", "application/json" )
            , ( "Content-Type", "application/json" )
            ]
        , url = url
        , body = Http.string (loginJSON model)
        }


postTitle : String -> Model -> Task.Task Http.RawError Http.Response
postTitle url model =
    let
        token =
            case model.token of
                Just token ->
                    token

                Nothing ->
                    ""
    in
        Http.send Http.defaultSettings
            { verb = "PATCH"
            , headers =
                [ ( "Accept", "application/json" )
                , ( "Content-Type", "application/json" )
                , ( "Authorization", "Bearer " ++ token )
                ]
            , url = url
            , body = Http.string (titleJSON model)
            }


decodeTitle : Json.Decoder String
decodeTitle =
    Json.at [ "title" ] Json.string


decodeToken : Json.Decoder String
decodeToken =
    Json.at [ "token" ] Json.string


updateTitle : Model -> Cmd Msg
updateTitle model =
    let
        url =
            "http://localhost:8080/Plone/front-page"
    in
        Task.perform UpdateFail
            UpdateSucceed
            (isEmptyResponse (postTitle url model))


isEmptyResponse : Task.Task Http.RawError Http.Response -> Task.Task Http.Error String
isEmptyResponse response =
    let
        isEmpty str =
            if str == "" then
                Task.succeed "ok"
            else
                Task.fail (Http.UnexpectedPayload "body is not empty")
    in
        Task.mapError promoteError response
            `Task.andThen` handleResponse isEmpty


handleResponse : (String -> Task.Task Http.Error a) -> Http.Response -> Task.Task Http.Error a
handleResponse handle response =
    if 200 <= response.status && response.status < 300 then
        case response.value of
            Http.Text str ->
                handle str

            _ ->
                Task.fail (Http.UnexpectedPayload "Response body is a blob, expecting a string.")
    else
        Task.fail (Http.BadResponse response.status response.statusText)


promoteError : Http.RawError -> Http.Error
promoteError rawError =
    case rawError of
        Http.RawTimeout ->
            Http.Timeout

        Http.RawNetworkError ->
            Http.NetworkError
