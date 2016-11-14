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
import Plone.Css as PloneCss
import Plone.Login as Login
import Plone.Page as Page
import RemoteData
import Return exposing (Return, mapBoth)


main =
    Html.App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { page : Page.Model
    , login : Login.Model
    , mdl :
        Material.Model
        -- Boilerplate: model store for any and all Mdl components you use.
    , debug : Bool
    }


localUrl =
    "http://localhost:8080/Plone/"


init : Return Msg Model
init =
    let
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
            , mdl = mdl
            , debug = False
            }
    in
        update (PageMsg Page.Fetch) initial_model



-- UPDATE


type Msg
    = Mdl (Material.Msg Msg)
    | LoginMsg Login.Msg
    | PageMsg Page.Msg


update : Msg -> Model -> Return Msg Model
update msg model =
    case Debug.log "model" msg of
        Mdl msg' ->
            Material.update msg' model

        LoginMsg msg' ->
            Login.update msg' model.login
                |> mapBoth LoginMsg (\login -> { model | login = login })

        PageMsg msg' ->
            Page.update msg' model.page model.login
                |> mapBoth PageMsg (\page -> { model | page = page })



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
    if model.login.connecting then
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
                , Button.render Mdl [ 0 ] model.mdl [ Button.onClick (PageMsg (Page.Update Page.Title)) ] [ text "Update" ]
                , Button.render Mdl [ 0 ] model.mdl [ Button.onClick (PageMsg Page.CancelInlineEdit) ] [ text "Cancel" ]
                ]
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
                , Button.render Mdl [ 0 ] model.mdl [ Button.onClick (PageMsg (Page.Update Page.Description)) ] [ text "Update" ]
                , Button.render Mdl [ 0 ] model.mdl [ Button.onClick (PageMsg Page.CancelInlineEdit) ] [ text "Cancel" ]
                ]
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
            , text (Login.userid model.login)
            , Button.render Mdl [ 0 ] model.mdl [ Button.onClick (LoginMsg Login.Logout) ] [ text "Logout" ]
            ]
    else
        Button.render Mdl [ 0 ] model.mdl [ Button.onClick (LoginMsg Login.LoginForm) ] [ text "Login" ]
