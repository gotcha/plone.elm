module Plone.Page exposing (..)

import HttpBuilder
import Json.Decode as Json
import Json.Decode exposing ((:=))
import Json.Encode exposing (object, string)
import Plone.Login exposing (Security)
import Task


-- MODEL


type Field
    = Title
    | Description
    | NoField


type alias PageModel =
    { title : String
    , description : String
    , inline_edit : Field
    , baseUrl : String
    }


changeField field value page =
    case field of
        Title ->
            { page | title = value }

        Description ->
            { page | description = value }

        NoField ->
            page



-- UPDATE


type PageMessage
    = Fetch
    | FetchSucceed (HttpBuilder.Response JsonPage)
    | FetchFail (HttpBuilder.Error String)
    | Change Field String
    | Update Field
    | UpdateSucceed (HttpBuilder.Response String)
    | UpdateFail (HttpBuilder.Error String)
    | InlineEdit Field
    | CancelInlineEdit


pageUpdate : PageMessage -> PageModel -> Security -> ( PageModel, Cmd PageMessage )
pageUpdate msg page sec =
    case msg of
        Fetch ->
            ( page, (getDocumentTitle page) )

        FetchSucceed response ->
            let
                page' =
                    { page
                        | title = response.data.title
                        , description = response.data.description
                    }
            in
                ( page', Cmd.none )

        FetchFail _ ->
            ( page, Cmd.none )

        Update field ->
            let
                page' =
                    { page | inline_edit = NoField }
            in
                ( page'
                , updateField field page sec
                )

        UpdateFail _ ->
            ( page, Cmd.none )

        UpdateSucceed _ ->
            ( page, Cmd.none )

        Change field value ->
            ( changeField field value page
            , Cmd.none
            )

        InlineEdit field ->
            let
                page' =
                    { page | inline_edit = field }
            in
                ( page', Cmd.none )

        CancelInlineEdit ->
            let
                page' =
                    { page | inline_edit = NoField }
            in
                ( page', Cmd.none )



-- HTTP


getDocumentTitle : PageModel -> Cmd PageMessage
getDocumentTitle page =
    Task.perform FetchFail
        FetchSucceed
        (fetchDocumentTitle page)


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


fetchDocumentTitle : PageModel -> Task.Task (HttpBuilder.Error String) (HttpBuilder.Response JsonPage)
fetchDocumentTitle page =
    let
        url =
            page.baseUrl ++ "front-page"
    in
        HttpBuilder.get url
            |> HttpBuilder.withHeader "Accept" "application/json"
            |> HttpBuilder.send (HttpBuilder.jsonReader decodePage) HttpBuilder.stringReader


postField : Field -> PageModel -> Security -> Task.Task (HttpBuilder.Error String) (HttpBuilder.Response String)
postField field page sec =
    let
        token =
            case sec.token of
                Just token ->
                    token

                Nothing ->
                    ""

        url =
            page.baseUrl ++ "front-page"
    in
        HttpBuilder.patch url
            |> HttpBuilder.withHeaders
                [ ( "Accept", "application/json" )
                , ( "Content-Type", "application/json" )
                , ( "Authorization", "Bearer " ++ token )
                ]
            |> HttpBuilder.withJsonBody (jsonField field page)
            |> HttpBuilder.send HttpBuilder.stringReader HttpBuilder.stringReader


jsonField field page =
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
                    page.title

                Description ->
                    page.description

                NoField ->
                    ""
    in
        (object [ ( name, string value ) ])


updateField : Field -> PageModel -> Security -> Cmd PageMessage
updateField field page sec =
    Task.perform UpdateFail
        UpdateSucceed
        (postField field page sec)
