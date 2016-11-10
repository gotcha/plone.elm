module Main exposing (..)

import Html
    exposing
        ( div
        , text
        , hr
        , h2
        , p
        , pre
        )
import Html.App
import Html.Events as Events
import Html.Attributes as Attr
import Task
import Http
import Return exposing (Return)
import Json.Decode as Json
import Json.Decode exposing ((:=))
import Json.Encode exposing (encode, object, string)
import Debug
import HttpBuilder
import Material
import Material.Scheme
import Material.Button as Button
import Material.Textfield as Textfield
import Material.Layout as Layout
import Material.Icon as Icon
import Material.Options exposing (css)
import Html.CssHelpers exposing (withNamespace)
import Plone.Css as PloneCss


main =
    Html.App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type Field
    = Title
    | Description
    | NoField


type alias Model =
    { title : String
    , description : String
    , inline_edit : Field
    , debug : Bool
    , sec : Security
    , baseUrl :
        String
    , mdl :
        Material.Model
        -- Boilerplate: model store for any and all Mdl components you use.
    }


type alias Security =
    { token : Maybe String
    , connecting : Bool
    , user : Maybe User
    }


type alias User =
    { userid : String
    , password : String
    }


init : Return Msg Model
init =
    let
        -- Boilerplate: Always use this initial Mdl model store.
        mdl =
            Material.model

        sec =
            { token = Nothing
            , connecting = False
            , user = Nothing
            }

        initial_model =
            Model "" "" NoField False sec "http://localhost:8080/Plone/" mdl
    in
        update Fetch initial_model


isLoggedIn : Model -> Bool
isLoggedIn model =
    case model.sec.token of
        Just token ->
            True

        Nothing ->
            False


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


login userid password =
    object
        [ ( "login", string userid )
        , ( "password", string password )
        ]


changeField field value model =
    case field of
        Title ->
            { model | title = value }

        Description ->
            { model | description = value }

        NoField ->
            model



-- UPDATE


type Msg
    = Fetch
    | FetchSucceed (HttpBuilder.Response Page)
    | FetchFail (HttpBuilder.Error String)
    | Change Field String
    | Update Field
    | UpdateSucceed (HttpBuilder.Response String)
    | UpdateFail (HttpBuilder.Error String)
    | Mdl (Material.Msg Msg)
    | InlineEdit Field
    | CancelInlineEdit
    | LoginMsg LoginMessage


type LoginMessage
    = LoginForm
    | CancelLoginForm
    | ChangePassword String
    | ChangeUserId String
    | LoggingIn
    | Logout
    | LoginSucceed (HttpBuilder.Response String)
    | LoginFail (HttpBuilder.Error String)


loginUpdate : LoginMessage -> Model -> ( Model, Cmd LoginMessage )
loginUpdate msg model =
    case msg of
        LoggingIn ->
            ( model, getToken model )

        Logout ->
            let
                sec' =
                    model.sec

                sec =
                    { sec' | token = Nothing }
            in
                ( { model | sec = sec }, Cmd.none )

        LoginSucceed response ->
            let
                sec' =
                    model.sec

                sec =
                    { sec'
                        | token = Just response.data
                        , connecting = False
                    }
            in
                ( { model | sec = sec }, Cmd.none )

        LoginFail _ ->
            let
                sec' =
                    model.sec

                sec =
                    { sec'
                        | connecting = False
                    }
            in
                ( { model | sec = sec }, Cmd.none )

        LoginForm ->
            let
                sec' =
                    model.sec

                sec =
                    { sec'
                        | connecting = True
                    }
            in
                ( { model | sec = sec }, Cmd.none )

        CancelLoginForm ->
            let
                sec' =
                    model.sec

                sec =
                    { sec'
                        | connecting = False
                        , user = Nothing
                    }
            in
                ( { model
                    | sec = sec
                  }
                , Cmd.none
                )

        ChangePassword newPassword ->
            let
                currentUserid =
                    userid model.sec

                sec' =
                    model.sec

                sec =
                    { sec'
                        | user = Just (User currentUserid newPassword)
                    }
            in
                ( { model
                    | sec = sec
                  }
                , Cmd.none
                )

        ChangeUserId newUserid ->
            let
                currentPassword =
                    password model.sec

                sec' =
                    model.sec

                sec =
                    { sec'
                        | user = Just (User newUserid currentPassword)
                    }
            in
                ( { model
                    | sec = sec
                  }
                , Cmd.none
                )


update : Msg -> Model -> Return Msg Model
update msg model =
    case Debug.log "model" msg of
        Fetch ->
            ( model, (getDocumentTitle model) )

        FetchSucceed response ->
            ( { model
                | title = response.data.title
                , description = response.data.description
              }
            , Cmd.none
            )

        FetchFail _ ->
            ( model, Cmd.none )

        Update field ->
            ( { model | inline_edit = NoField }
            , updateField field model
            )

        UpdateFail _ ->
            ( model, Cmd.none )

        UpdateSucceed _ ->
            ( model, Cmd.none )

        Change field value ->
            ( changeField field value model
            , Cmd.none
            )

        InlineEdit field ->
            ( { model | inline_edit = field }
            , Cmd.none
            )

        CancelInlineEdit ->
            ( { model | inline_edit = NoField }
            , Cmd.none
            )

        -- When the `Mdl` messages come through, update appropriately.
        Mdl msg' ->
            Material.update msg' model

        LoginMsg msg' ->
            let
                ( model', cmd' ) =
                    loginUpdate msg' model
            in
                ( model', Cmd.map LoginMsg cmd' )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


{ id, class, classList } =
    withNamespace "plone"
type alias Mdl =
    Material.Model


view : Model -> Html.Html Msg
view model =
    if model.sec.connecting then
        loginFormView model
            |> Material.Scheme.top
    else
        mainView model
            |> Material.Scheme.top


userOnInput string =
    LoginMsg (ChangeUserId string)


passwordOnInput string =
    LoginMsg (ChangePassword string)


loginFormView model =
    div []
        [ h2 [] [ text "Login Form" ]
        , Textfield.render Mdl
            [ 0 ]
            model.mdl
            [ Textfield.label "User Id"
            , Textfield.floatingLabel
            , Textfield.autofocus
            , Textfield.text'
            , Textfield.onInput userOnInput
            ]
        , Textfield.render Mdl
            [ 1 ]
            model.mdl
            [ Textfield.label "Password"
            , Textfield.floatingLabel
            , Textfield.password
            , Textfield.onInput passwordOnInput
            ]
        , Button.render Mdl [ 0 ] model.mdl [ Button.onClick (LoginMsg LoggingIn) ] [ text "Login" ]
        , Button.render Mdl [ 0 ] model.mdl [ Button.onClick (LoginMsg CancelLoginForm) ] [ text "Cancel" ]
        , debugView model
        ]


mainView model =
    Layout.render Mdl
        model.mdl
        [ Layout.fixedHeader ]
        { header = [ loginView model ]
        , drawer = []
        , tabs = ( [], [] )
        , main = [ titleView model, descriptionView model, debugView model ]
        }


titleView model =
    let
        editButton =
            if isLoggedIn model then
                Button.render Mdl
                    [ 0 ]
                    model.mdl
                    [ Button.icon
                    , Button.onClick (InlineEdit Title)
                    ]
                    [ Icon.i "mode_edit" ]
            else
                text ""

        updateSnippet =
            if isLoggedIn model then
                updateTitleView model
            else
                text ""

        titleWidget =
            if model.inline_edit == Title then
                updateSnippet
            else
                h2 []
                    [ text model.title
                    , editButton
                    ]
    in
        div [] [ titleWidget ]


updateTitleView model =
    div [ Attr.class "updateTitleWidget" ]
        [ Textfield.render Mdl
            [ 0 ]
            model.mdl
            [ Textfield.label "Title"
            , Textfield.floatingLabel
            , Textfield.autofocus
            , Textfield.text'
            , Textfield.onInput (Change Title)
            , Textfield.value model.title
            ]
        , Button.render Mdl [ 0 ] model.mdl [ Button.onClick (Update Title) ] [ text "Update" ]
        , Button.render Mdl [ 0 ] model.mdl [ Button.onClick CancelInlineEdit ] [ text "Cancel" ]
        ]


descriptionView model =
    let
        editButton =
            if isLoggedIn model then
                Button.render Mdl
                    [ 0 ]
                    model.mdl
                    [ Button.icon
                    , Button.onClick (InlineEdit Description)
                    ]
                    [ Icon.i "mode_edit" ]
            else
                text ""

        updateSnippet =
            if isLoggedIn model then
                updateDescriptionView model
            else
                text ""

        descriptionWidget =
            if model.inline_edit == Description then
                updateSnippet
            else
                p []
                    [ text model.description
                    , editButton
                    ]
    in
        div [] [ descriptionWidget ]


updateDescriptionView model =
    div [ Attr.class "updateDescriptionWidget" ]
        [ Textfield.render Mdl
            [ 0 ]
            model.mdl
            [ Textfield.label "Description"
            , Textfield.floatingLabel
            , Textfield.autofocus
            , Textfield.text'
            , Textfield.onInput (Change Description)
            , Textfield.value model.description
            ]
        , Button.render Mdl [ 0 ] model.mdl [ Button.onClick (Update Description) ] [ text "Update" ]
        , Button.render Mdl [ 0 ] model.mdl [ Button.onClick CancelInlineEdit ] [ text "Cancel" ]
        ]


debugView model =
    if model.debug then
        div []
            [ hr [] []
            , text (toString model)
            , hr [] []
            ]
    else
        text ""


loginView model =
    if isLoggedIn model then
        div [ class [ PloneCss.NavBar ] ]
            [ Icon.i "person"
            , text (userid model.sec)
            , Button.render Mdl [ 0 ] model.mdl [ Button.onClick (LoginMsg Logout) ] [ text "Logout" ]
            ]
    else
        Button.render Mdl [ 0 ] model.mdl [ Button.onClick (LoginMsg LoginForm) ] [ text "Login" ]



-- HTTP


getDocumentTitle : Model -> Cmd Msg
getDocumentTitle model =
    Task.perform FetchFail
        FetchSucceed
        (fetchDocumentTitle model)


type alias Page =
    { title : String
    , description : String
    }


decodePage : Json.Decoder Page
decodePage =
    let
        decodeDescription =
            Json.at [ "description", "data" ] Json.string
    in
        Json.object2 Page
            ("title" := Json.string)
            decodeDescription


fetchDocumentTitle : Model -> Task.Task (HttpBuilder.Error String) (HttpBuilder.Response Page)
fetchDocumentTitle model =
    let
        url =
            ploneUrl model "front-page"
    in
        HttpBuilder.get url
            |> HttpBuilder.withHeader "Accept" "application/json"
            |> HttpBuilder.send (HttpBuilder.jsonReader decodePage) HttpBuilder.stringReader


ploneUrl : Model -> String -> String
ploneUrl model path =
    model.baseUrl ++ path


getToken : Model -> Cmd LoginMessage
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
            |> HttpBuilder.withJsonBody (login (userid model.sec) (password model.sec))
            |> HttpBuilder.send (HttpBuilder.jsonReader decodeToken) HttpBuilder.stringReader


postField : Field -> Model -> Task.Task (HttpBuilder.Error String) (HttpBuilder.Response String)
postField field model =
    let
        token =
            case model.sec.token of
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
            |> HttpBuilder.withJsonBody (jsonField field model)
            |> HttpBuilder.send HttpBuilder.stringReader HttpBuilder.stringReader


jsonField field model =
    let
        name =
            case field of
                Title ->
                    "title"

                Description ->
                    "description"

                NoField ->
                    "no_field"

        value =
            case field of
                Title ->
                    model.title

                Description ->
                    model.description

                NoField ->
                    ""
    in
        (object [ ( name, string value ) ])


updateField : Field -> Model -> Cmd Msg
updateField field model =
    Task.perform UpdateFail
        UpdateSucceed
        (postField field model)
