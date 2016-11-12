module Plone.Page exposing (..)

import HttpBuilder
import Json.Decode as Json
import Json.Decode exposing ((:=))
import Json.Encode exposing (object, string)
import Plone.Login as Login
import Task


-- MODEL


type Field
    = Title
    | Description
    | NoField


type alias Model =
    { title : String
    , description : String
    , inline_edit : Field
    , baseUrl : String
    }


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
    | FetchSucceed (HttpBuilder.Response JsonPage)
    | FetchFail (HttpBuilder.Error String)
    | Change Field String
    | Update Field
    | UpdateSucceed (HttpBuilder.Response String)
    | UpdateFail (HttpBuilder.Error String)
    | InlineEdit Field
    | CancelInlineEdit


update : Msg -> Model -> Login.Model -> ( Model, Cmd Msg )
update msg model login =
    case msg of
        Fetch ->
            ( model, (getDocumentTitle model) )

        FetchSucceed response ->
            let
                model' =
                    { model
                        | title = response.data.title
                        , description = response.data.description
                    }
            in
                ( model', Cmd.none )

        FetchFail _ ->
            ( model, Cmd.none )

        Update field ->
            let
                model' =
                    { model | inline_edit = NoField }
            in
                ( model'
                , updateField field model login
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
                model' =
                    { model | inline_edit = field }
            in
                ( model', Cmd.none )

        CancelInlineEdit ->
            let
                model' =
                    { model | inline_edit = NoField }
            in
                ( model', Cmd.none )



-- HTTP


getDocumentTitle : Model -> Cmd Msg
getDocumentTitle model =
    Task.perform FetchFail
        FetchSucceed
        (fetchDocumentTitle model)


type alias JsonPage =
    { title : String
    , description : String
    }


decodePage : Json.Decoder JsonPage
decodePage =
    let
        decodeDescription =
            Json.at [ "description", "data" ] Json.string
    in
        Json.object2 JsonPage
            ("title" := Json.string)
            decodeDescription


fetchDocumentTitle : Model -> Task.Task (HttpBuilder.Error String) (HttpBuilder.Response JsonPage)
fetchDocumentTitle model =
    let
        url =
            model.baseUrl ++ "front-page"
    in
        HttpBuilder.get url
            |> HttpBuilder.withHeader "Accept" "application/json"
            |> HttpBuilder.send (HttpBuilder.jsonReader decodePage) HttpBuilder.stringReader


postField : Field -> Model -> Login.Model -> Task.Task (HttpBuilder.Error String) (HttpBuilder.Response String)
postField field model login =
    let
        token =
            case login.token of
                Just token ->
                    token

                Nothing ->
                    ""

        url =
            model.baseUrl ++ "front-page"
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


updateField : Field -> Model -> Login.Model -> Cmd Msg
updateField field model login =
    Task.perform UpdateFail
        UpdateSucceed
        (postField field model login)
