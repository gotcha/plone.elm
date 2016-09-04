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
import HttpBuilder


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
    , baseUrl : String
    }


type alias User =
    { userid : String
    , password : String
    }


init : String -> ( Model, Cmd Msg )
init title =
    ( Model title Nothing False Nothing "http://localhost:8080/Plone/"
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



-- UPDATE


type Msg
    = Fetch
    | FetchSucceed (HttpBuilder.Response String)
    | FetchFail (HttpBuilder.Error String)
    | LoginForm
    | CancelLoginForm
    | ChangePassword String
    | ChangeUserId String
    | ChangeTitle String
    | UpdateTitle
    | LoggingIn
    | LoginSucceed (HttpBuilder.Response String)
    | LoginFail (HttpBuilder.Error String)
    | UpdateSucceed (HttpBuilder.Response String)
    | UpdateFail (HttpBuilder.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "model" msg of
        Fetch ->
            ( model, (getDocumentTitle model) )

        FetchSucceed response ->
            ( { model | title = response.data }, Cmd.none )

        FetchFail _ ->
            ( model, Cmd.none )

        LoggingIn ->
            ( model, getToken model )

        UpdateTitle ->
            ( model, updateTitle model )

        LoginSucceed response ->
            ( { model
                | token = Just response.data
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
        ]


loginView model =
    if isLoggedIn model then
        div [] [ text (userid model) ]
    else
        button [ Events.onClick LoginForm ] [ text "Login" ]



-- HTTP


getDocumentTitle : Model -> Cmd Msg
getDocumentTitle model =
    Task.perform FetchFail
        FetchSucceed
        (fetchDocumentTitle model)


decodeTitle : Json.Decoder String
decodeTitle =
    Json.at [ "title" ] Json.string


fetchDocumentTitle : Model -> Task.Task (HttpBuilder.Error String) (HttpBuilder.Response String)
fetchDocumentTitle model =
    let
        url =
            ploneUrl model "front-page"
    in
        HttpBuilder.get url
            |> HttpBuilder.withHeader "Accept" "application/json"
            |> HttpBuilder.send (HttpBuilder.jsonReader decodeTitle) HttpBuilder.stringReader


ploneUrl : Model -> String -> String
ploneUrl model path =
    model.baseUrl ++ path


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
            ploneUrl model "@login"
    in
        HttpBuilder.post url
            |> HttpBuilder.withHeaders
                [ ( "Accept", "application/json" )
                , ( "Content-Type", "application/json" )
                ]
            |> HttpBuilder.withJsonBody (login (userid model) (password model))
            |> HttpBuilder.send (HttpBuilder.jsonReader decodeToken) HttpBuilder.stringReader


postTitle : Model -> Task.Task (HttpBuilder.Error String) (HttpBuilder.Response String)
postTitle model =
    let
        token =
            case model.token of
                Just token ->
                    token

                Nothing ->
                    ""

        url =
            ploneUrl model "front-page"
    in
        HttpBuilder.patch url
            |> HttpBuilder.withHeaders
                [ ( "Accept", "application/json" )
                , ( "Content-Type", "application/json" )
                , ( "Authorization", "Bearer " ++ token )
                ]
            |> HttpBuilder.withJsonBody (object [ ( "title", string model.title ) ])
            |> HttpBuilder.send HttpBuilder.stringReader HttpBuilder.stringReader


updateTitle : Model -> Cmd Msg
updateTitle model =
    Task.perform UpdateFail
        UpdateSucceed
        (postTitle model)
