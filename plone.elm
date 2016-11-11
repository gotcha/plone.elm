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
import Plone.Login exposing (..)


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
    { page : PageModel
    , sec : Security
    , mdl :
        Material.Model
        -- Boilerplate: model store for any and all Mdl components you use.
    , debug : Bool
    }


type alias PageModel =
    { title : String
    , description : String
    , inline_edit : Field
    , baseUrl : String
    }


localUrl =
    "http://localhost:8080/Plone/"


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
            , baseUrl = localUrl
            }

        page =
            { title = ""
            , description = ""
            , inline_edit = NoField
            , baseUrl = localUrl
            }

        initial_model =
            Model page sec mdl False
    in
        update Fetch initial_model


changeField field value model =
    let
        page' =
            model.page

        page =
            case field of
                Title ->
                    { page' | title = value }

                Description ->
                    { page' | description = value }

                NoField ->
                    page'
    in
        { model | page = page }



-- UPDATE


type Msg
    = Mdl (Material.Msg Msg)
    | LoginMsg LoginMessage
    | Fetch
    | FetchSucceed (HttpBuilder.Response Page)
    | FetchFail (HttpBuilder.Error String)
    | Change Field String
    | Update Field
    | UpdateSucceed (HttpBuilder.Response String)
    | UpdateFail (HttpBuilder.Error String)
    | InlineEdit Field
    | CancelInlineEdit


update : Msg -> Model -> Return Msg Model
update msg model =
    case Debug.log "model" msg of
        Fetch ->
            ( model, (getDocumentTitle model) )

        FetchSucceed response ->
            let
                page' =
                    model.page

                page =
                    { page'
                        | title = response.data.title
                        , description = response.data.description
                    }
            in
                ( { model | page = page }, Cmd.none )

        FetchFail _ ->
            ( model, Cmd.none )

        Update field ->
            let
                page' =
                    model.page

                page =
                    { page' | inline_edit = NoField }
            in
                ( { model | page = page }
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
            let
                page' =
                    model.page

                page =
                    { page' | inline_edit = field }
            in
                ( { model | page = page }
                , Cmd.none
                )

        CancelInlineEdit ->
            let
                page' =
                    model.page

                page =
                    { page' | inline_edit = NoField }
            in
                ( { model | page = page }
                , Cmd.none
                )

        -- When the `Mdl` messages come through, update appropriately.
        Mdl msg' ->
            Material.update msg' model

        LoginMsg msg' ->
            let
                ( sec', cmd' ) =
                    loginUpdate msg' model.sec

                model =
                    { model | sec = sec' }
            in
                ( model, Cmd.map LoginMsg cmd' )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


{ id, class, classList } =
    withNamespace "plone"
type alias Mdl =
    Material.Model


isLoggedIn : Model -> Bool
isLoggedIn model =
    case model.sec.token of
        Just token ->
            True

        Nothing ->
            False


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
            if model.page.inline_edit == Title then
                updateSnippet
            else
                h2 []
                    [ text model.page.title
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
            , Textfield.value model.page.title
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
            if model.page.inline_edit == Description then
                updateSnippet
            else
                p []
                    [ text model.page.description
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
            , Textfield.value model.page.description
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
    model.page.baseUrl ++ path


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
                    model.page.title

                Description ->
                    model.page.description

                NoField ->
                    ""
    in
        (object [ ( name, string value ) ])


updateField : Field -> Model -> Cmd Msg
updateField field model =
    Task.perform UpdateFail
        UpdateSucceed
        (postField field model)
