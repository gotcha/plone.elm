module Main exposing (..)

import Debug
import Html
    exposing
        ( div
        , text
        , hr
        , h2
        , p
        , pre
        , form
        )
import Html.App
import Html.Attributes as Attr
import Html.CssHelpers exposing (withNamespace)
import Html.Events as Events
import Material
import Material.Scheme
import Material.Button as Button
import Material.Textfield as Textfield
import Material.Layout as Layout
import Material.Icon as Icon
import Material.Options exposing (css)
import Navigation
import Plone.Css as PloneCss
import Plone.Login as Login
import Plone.Page as Page
import RemoteData
import Return exposing (Return, mapBoth, singleton, command)
import String
import UrlParser


main =
    Navigation.program (Navigation.makeParser hashParser)
        { init = init
        , subscriptions = subscriptions
        , update = update
        , urlUpdate = urlUpdate
        , view = view
        }



-- NAVIGATION


hashParser : Navigation.Location -> Result String Route
hashParser location =
    let
        dummy =
            Debug.log "location" location
    in
        UrlParser.parse identity routeParser (String.dropLeft 1 location.hash)


type Route
    = LoginRoute
    | HomeRoute


routeParser : UrlParser.Parser (Route -> a) a
routeParser =
    UrlParser.oneOf
        [ UrlParser.format LoginRoute (UrlParser.s "login")
        , UrlParser.format HomeRoute (UrlParser.s "home")
        , UrlParser.format LoginRoute (UrlParser.s "")
        ]


urlUpdate : Result String Route -> Model -> ( Model, Cmd Msg )
urlUpdate result model =
    case Debug.log "urlUpdate" result of
        Err _ ->
            singleton model

        Ok route ->
            let
                model_ =
                    { model | route = route }
            in
                case route of
                    LoginRoute ->
                        Login.update Login.LoginForm model.login
                            |> mapBoth LoginMsg (\login -> { model_ | login = login })

                    HomeRoute ->
                        Page.update Page.Fetch model.page model.login
                            |> mapBoth PageMsg (\page -> { model_ | page = page })



-- MODEL


type alias Model =
    { page : Page.Model
    , login : Login.Model
    , route : Route
    , mdl :
        Material.Model
        -- Boilerplate: model store for any and all Mdl components you use.
    , debug : Bool
    }


localUrl =
    "http://localhost:8080/Plone/"


init : Result String Route -> Return Msg Model
init route =
    let
        route =
            Debug.log "route" route

        -- Boilerplate: Always use this initial Mdl model store.
        mdl =
            Material.model

        login =
            { token = Nothing
            , connecting = False
            , user = Nothing
            , baseUrl = localUrl
            }

        page =
            { title = RemoteData.Loading
            , description = RemoteData.Loading
            , inline_edit = Page.NoField
            , baseUrl = localUrl
            }

        initial_model =
            { page = page
            , login = login
            , route = LoginRoute
            , mdl = mdl
            , debug = False
            }
    in
        urlUpdate route initial_model



-- UPDATE


type Msg
    = Mdl (Material.Msg Msg)
    | LoginMsg Login.Msg
    | PageMsg Page.Msg
    | LoginPage


update : Msg -> Model -> Return Msg Model
update msg model =
    case Debug.log "update" msg of
        Mdl msg_ ->
            Material.update msg_ model

        LoginMsg msg_ ->
            Login.update msg_ model.login
                |> mapBoth LoginMsg (\login -> { model | login = login })

        PageMsg msg_ ->
            Page.update msg_ model.page model.login
                |> mapBoth PageMsg (\page -> { model | page = page })

        LoginPage ->
            singleton model
                |> command (Navigation.newUrl "#login")



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
    case model.login.token of
        Just token ->
            True

        Nothing ->
            False


view : Model -> Html.Html Msg
view model =
    if model.route == LoginRoute then
        loginFormView model
            |> Material.Scheme.top
    else
        mainView model
            |> Material.Scheme.top


loginFormView model =
    div []
        [ h2 [] [ text "Login Form" ]
        , form [ Events.onSubmit (LoginMsg Login.GetToken) ]
            [ Textfield.render Mdl
                [ 0 ]
                model.mdl
                [ Textfield.label "User Id"
                , Textfield.floatingLabel
                , Textfield.autofocus
                , Textfield.text'
                , Textfield.onInput (\str -> LoginMsg (Login.ChangeUserId str))
                ]
            , Textfield.render Mdl
                [ 1 ]
                model.mdl
                [ Textfield.label "Password"
                , Textfield.floatingLabel
                , Textfield.password
                , Textfield.onInput (\str -> LoginMsg (Login.ChangePassword str))
                ]
            , Button.render Mdl [ 0 ] model.mdl [ Button.onClick (LoginMsg Login.GetToken) ] [ text "Login" ]
            , Button.render Mdl [ 0 ] model.mdl [ Button.onClick (LoginMsg Login.CancelLoginForm) ] [ text "Cancel" ]
            ]
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
                    , Button.onClick (PageMsg (Page.InlineEdit Page.Title))
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
            case model.page.title of
                RemoteData.Success value ->
                    if model.page.inline_edit == Page.Title then
                        updateSnippet
                    else
                        h2 []
                            [ text value
                            , editButton
                            ]

                RemoteData.Failure err ->
                    text ("Error: " ++ toString err)

                RemoteData.NotAsked ->
                    text ""

                RemoteData.Loading ->
                    text "Loading..."
    in
        div [] [ titleWidget ]


updateTitleView model =
    let
        title =
            case model.page.title of
                RemoteData.Success value ->
                    value

                _ ->
                    ""
    in
        div [ Attr.class "updateTitleWidget" ]
            [ form [ Events.onSubmit (PageMsg (Page.Update Page.Title)) ]
                [ Textfield.render Mdl
                    [ 0 ]
                    model.mdl
                    [ Textfield.label "Title"
                    , Textfield.floatingLabel
                    , Textfield.autofocus
                    , Textfield.text'
                    , Textfield.onInput (\str -> PageMsg (Page.Change Page.Title str))
                    , Textfield.value title
                    ]
                , Button.render Mdl [ 0 ] model.mdl [] [ text "Update" ]
                ]
            , Button.render Mdl [ 0 ] model.mdl [ Button.onClick (PageMsg Page.CancelInlineEdit) ] [ text "Cancel" ]
            ]


descriptionView model =
    let
        editButton =
            if isLoggedIn model then
                Button.render Mdl
                    [ 0 ]
                    model.mdl
                    [ Button.icon
                    , Button.onClick (PageMsg (Page.InlineEdit Page.Description))
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
            case model.page.description of
                RemoteData.Success value ->
                    if model.page.inline_edit == Page.Description then
                        updateSnippet
                    else
                        p []
                            [ text value
                            , editButton
                            ]

                RemoteData.Failure err ->
                    text ("Error: " ++ toString err)

                RemoteData.NotAsked ->
                    text ""

                RemoteData.Loading ->
                    text "Loading..."
    in
        div [] [ descriptionWidget ]


updateDescriptionView model =
    let
        description =
            case model.page.description of
                RemoteData.Success value ->
                    value

                _ ->
                    ""
    in
        div [ Attr.class "updateDescriptionWidget" ]
            [ form [ Events.onSubmit (PageMsg (Page.Update Page.Description)) ]
                [ Textfield.render Mdl
                    [ 0 ]
                    model.mdl
                    [ Textfield.label "Description"
                    , Textfield.floatingLabel
                    , Textfield.autofocus
                    , Textfield.text'
                    , Textfield.onInput (\str -> PageMsg (Page.Change Page.Description str))
                    , Textfield.value description
                    ]
                , Button.render Mdl [ 0 ] model.mdl [] [ text "Update" ]
                ]
            , Button.render Mdl [ 0 ] model.mdl [ Button.onClick (PageMsg Page.CancelInlineEdit) ] [ text "Cancel" ]
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
        Layout.row []
            [ Layout.title [] [ text "Plone.elm" ]
            , Layout.spacer
            , Layout.navigation []
                [ Layout.link [] [ Icon.i "person", text (Login.userid model.login) ]
                , Layout.link [ Layout.onClick (LoginMsg Login.Logout), css "cursor" "pointer" ] [ text "Logout" ]
                ]
            ]
    else
        Layout.row []
            [ Layout.title [] [ text "Plone.elm" ]
            , Layout.spacer
            , Layout.navigation []
                [ Layout.link [ Layout.onClick LoginPage, css "cursor" "pointer" ] [ text "Login" ]
                ]
            ]
