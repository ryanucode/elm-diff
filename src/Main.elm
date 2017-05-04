module Main exposing (..)

import Html exposing (..)
import Html.Attributes as Attribute exposing (property)
import Html.Events as Event exposing (on, onInput)
import Json.Decode as Decode
import Json.Encode as Encode
import Task

import Diff exposing (diff)

import Firebase
import Firebase.Database as Database
import Firebase.Database.Types exposing (..)
import Firebase.Database.Reference as Reference
import Firebase.Database.Snapshot as Snapshot

main : Program Never Model Msg
main = Html.program
    { init = initModel ! []
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

type alias Model = 
    { content : String
    , selection : (Int, Int)
    }

initModel =
    { content = ""
    , selection = (0, 0)
    }

app =
    case Firebase.app () of
        Just app ->
            app
        Nothing ->
            Debug.crash "could not initialize firebase"

database : Database
database =
    Database.init app

ref : Reference
ref = Database.ref (Just "/") database

type Msg
    = Type String
    | Receive String
    | SetSelection Int Int
    | Log String
    | NoOp

resultMsg : (x -> msg) -> (a -> msg) -> Result x a -> msg
resultMsg failMsg msg result =
    case result of
        Err error ->
            failMsg error
        Ok x ->
            msg x

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Type content ->
            Debug.log "typing" <|
            ( { model | content = content }
            , Reference.set (Encode.string content) ref
                |> Task.attempt (\_ -> NoOp)
            )

        Receive content ->
            { model | content = content } ! []

        SetSelection start end ->
            { model | selection = (start, end) } ! []

        Log error ->
            Debug.log "log" error
                |> \_ -> model ! []
        NoOp ->
            model ! []

view : Model -> Html Msg
view model =
    textarea
        [ onInput Type
        , on "change"
            <| Decode.map2 (\start end -> SetSelection start end)
                (Decode.at ["target", "selectionStart"] Decode.int)
                (Decode.at ["target", "selectionEnd"] Decode.int)
        , property "value" <| Encode.string model.content
        ]
        [text model.content]

subscriptions : Model -> Sub Msg
subscriptions model =
    Reference.on "value" ref
        <| \snapshot ->
            case Decode.decodeValue Decode.string <| Snapshot.exportVal snapshot of
                Ok string ->
                    if string == model.content then NoOp else Receive string
                Err error ->
                    Log error
