module Messages exposing (Message, Messages, Msg, addUser, requestMessages, startGame, update)

import Http
import Json.Decode
import Json.Encode
import Task
import Time


url =
    "https://retrospective-17367.firebaseio.com/messages.json"


type Msg
    = GetMessages
    | GotMessages (Result Http.Error (List Message))
    | PrepareMessage String
    | SendMessage Message
    | Sent (Result Http.Error ())


type alias Messages =
    Maybe (List Message)


type alias Message =
    { value : String
    , time : Time.Posix
    }


startGame : Msg
startGame =
    PrepareMessage "START"


addUser : String -> Msg
addUser userName =
    PrepareMessage ("USER " ++ userName)


requestMessages =
    GetMessages


prepareMessage : String -> Cmd Msg
prepareMessage message =
    Task.perform SendMessage (Time.now |> Task.andThen (\time -> Task.succeed { value = message, time = time }))


getMessages : Cmd Msg
getMessages =
    Http.get
        { url = url
        , expect = Http.expectJson GotMessages decodeMessages
        }


postMessage : Message -> Cmd Msg
postMessage message =
    Http.post
        { url = url
        , body = message |> encodeMessage |> Http.jsonBody
        , expect = Http.expectWhatever Sent
        }


encodeMessage : Message -> Json.Encode.Value
encodeMessage message =
    Json.Encode.object
        [ ( "value", Json.Encode.string message.value )
        , ( "time", Json.Encode.int (Time.posixToMillis message.time) )
        ]


decodeMessages : Json.Decode.Decoder (List Message)
decodeMessages =
    Json.Decode.map (List.map (\( _, message ) -> message)) (Json.Decode.keyValuePairs decodeMessage)


decodeMessage : Json.Decode.Decoder Message
decodeMessage =
    Json.Decode.map2 Message (Json.Decode.field "value" Json.Decode.string) (Json.Decode.field "time" (Json.Decode.map Time.millisToPosix Json.Decode.int))


update : Msg -> Messages -> ( Messages, Cmd Msg )
update msg messages =
    case msg of
        GetMessages ->
            ( messages, getMessages )

        GotMessages messagesResult ->
            case messagesResult of
                Ok messageList ->
                    let
                        sorted =
                            List.sortBy (\message -> Time.posixToMillis message.time) messageList
                    in
                    ( Just sorted, Cmd.none )

                Err err ->
                    ( Nothing, Cmd.none )

        PrepareMessage message ->
            ( messages, prepareMessage message )

        SendMessage message ->
            ( messages, postMessage message )

        Sent _ ->
            ( messages, Cmd.none )
