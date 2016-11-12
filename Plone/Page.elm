module Plone.Page exposing (..)

import HttpBuilder
import Json.Decode as Json
import Json.Decode exposing ((:=))
import Json.Encode exposing (object, string)
import Plone.Login as Login
import RemoteData exposing (WebData)
import Return exposing (Return, singleton, command)
import Task


-- MODEL


type Field
    = Title
    | Description
    | NoField


type alias Model =
    { title : WebData String
    , description : WebData String
    , inline_edit : Field
    , baseUrl : String
    }


changeField field value model =
    case field of
        Title ->
            { model | title = RemoteData.Success value }

        Description ->
            { model | description = RemoteData.Success value }

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


update : Msg -> Model -> Login.Model -> Return Msg Model
update msg model login =
    case msg of
        Fetch ->
            singleton model
                |> command (fetchCmd model)

        FetchSucceed response ->
            singleton
                { model
                    | title = RemoteData.Success response.data.title
                    , description = RemoteData.Success response.data.description
                }

        FetchFail _ ->
            singleton model

        Update field ->
            singleton { model | inline_edit = NoField }
                |> command (updateFieldCmd field model login)

        UpdateFail _ ->
            singleton model

        UpdateSucceed _ ->
            singleton model

        Change field value ->
            singleton (changeField field value model)

        InlineEdit field ->
            singleton { model | inline_edit = field }

        CancelInlineEdit ->
            singleton { model | inline_edit = NoField }



-- HTTP


fetchCmd : Model -> Cmd Msg
fetchCmd model =
    Task.perform FetchFail FetchSucceed (fetchRequest model)


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


fetchRequest : Model -> Task.Task (HttpBuilder.Error String) (HttpBuilder.Response JsonPage)
fetchRequest model =
    let
        url =
            model.baseUrl ++ "front-page"
    in
        HttpBuilder.get url
            |> HttpBuilder.withHeader "Accept" "application/json"
            |> HttpBuilder.send (HttpBuilder.jsonReader decodePage) HttpBuilder.stringReader


updateFieldPost : Field -> Model -> Login.Model -> Task.Task (HttpBuilder.Error String) (HttpBuilder.Response String)
updateFieldPost field model login =
    let
        token =
            Maybe.withDefault "" login.token

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
                    case model.title of
                        RemoteData.Success value ->
                            value

                        _ ->
                            ""

                Description ->
                    case model.description of
                        RemoteData.Success value ->
                            value

                        _ ->
                            ""

                NoField ->
                    ""
    in
        (object [ ( name, string value ) ])


updateFieldCmd : Field -> Model -> Login.Model -> Cmd Msg
updateFieldCmd field model login =
    Task.perform UpdateFail
        UpdateSucceed
        (updateFieldPost field model login)
