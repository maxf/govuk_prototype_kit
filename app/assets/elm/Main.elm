module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Events exposing (..)
import Http
import Json.Decode exposing (..)
import Task
import Dict exposing (..)


main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


-- MODEL


type alias Register =
    { text : String
    , register : String
    , registry : String
    }


type alias RegisterDict =
    Dict String Register


type alias Model =
    RegisterDict


init : ( Model, Cmd Msg )
init =
    ( empty
    , getRegisterList
    )


-- UPDATE


type Msg
    = FetchNow
    | FetchSucceed Model
    | FetchFail Http.Error


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchNow ->
            ( model, getRegisterList )

        FetchSucceed registerDict ->
            ( registerDict, Cmd.none )

        FetchFail error ->
            ( model, Cmd.none )


getRegisterList : Cmd Msg
getRegisterList =
    let
        url =
            "https://register.register.gov.uk/records.json"
    in
        Task.perform
            FetchFail
            FetchSucceed
            (Http.get registerDictDecoder url)


registerDictDecoder : Decoder RegisterDict
registerDictDecoder =
    dict
        (object3 Register
            ("text" := string)
            ("register" := string)
            ("registry" := string)
        )


-- VIEW


viewRecord : ( String, Register ) -> Html Msg
viewRecord ( key, record ) =
    li []
        [ text (key ++ ": " ++ record.text) ]


viewListOfRecords : RegisterDict -> Html Msg
viewListOfRecords records =
    ul []
        (List.map viewRecord (toList records))


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Registers" ]
        , viewListOfRecords model
        , button [ onClick FetchNow ] [ text "Fetch" ]
        ]
