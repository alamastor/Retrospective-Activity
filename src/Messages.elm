module Messages exposing (Message, Messages, Msg, addUser, messageTypeToString, prepareMessage, requestMessages, update)

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
    | SendMessage Message
    | Sent (Result Http.Error ())


type alias Messages =
    List Message


type alias Message =
    { value : MessageType
    , time : Time.Posix
    }


type MessageType
    = InvalidMessage
    | Start
    | AddUser String
    | UsersReady


messageTypeToString : MessageType -> String
messageTypeToString messageType =
    case messageType of
        InvalidMessage ->
            "?"

        Start ->
            "START"

        AddUser user ->
            "ADD_USER " ++ user

        UsersReady ->
            "USERS_READY"


addUser : String -> Cmd Msg
addUser userName =
    AddUser userName |> prepareMessage


requestMessages =
    GetMessages


prepareMessage : MessageType -> Cmd Msg
prepareMessage message =
    Task.perform SendMessage
        (Time.now |> Task.andThen (\time -> Task.succeed { value = message, time = time }))


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
        [ ( "value", messageTypeToString message.value |> Json.Encode.string )
        , ( "time", Json.Encode.int (Time.posixToMillis message.time) )
        ]


decodeMessages : Json.Decode.Decoder (List Message)
decodeMessages =
    Json.Decode.map (List.map (\( _, message ) -> message)) (Json.Decode.keyValuePairs decodeMessage)


decodeMessage : Json.Decode.Decoder Message
decodeMessage =
    Json.Decode.map2 Message
        (Json.Decode.field "value" (Json.Decode.map decodeMessageStr Json.Decode.string))
        (Json.Decode.field "time" (Json.Decode.map Time.millisToPosix Json.Decode.int))


decodeMessageStr : String -> MessageType
decodeMessageStr string =
    let
        itemList =
            String.split " " string
    in
    case itemList of
        [] ->
            InvalidMessage

        msgType :: values ->
            if msgType == "START" then
                Start

            else if msgType == "ADD_USER" then
                case values of
                    [] ->
                        InvalidMessage

                    userName :: stuff ->
                        AddUser userName

            else if msgType == "USERS_READY" then
                UsersReady

            else
                InvalidMessage


update : Msg -> Messages -> ( Messages, Cmd Msg )
update msg messages =
    case msg of
        GetMessages ->
            ( messages, getMessages )

        GotMessages messageList ->
            case messageList of
                Ok messageListOk ->
                    let
                        sorted =
                            List.sortBy (\message -> Time.posixToMillis message.time) messageListOk
                    in
                    ( List.filter isValid sorted, Cmd.none )

                Err _ ->
                    ( messages, Cmd.none )

        SendMessage message ->
            ( messages ++ [ message ], postMessage message )

        Sent _ ->
            ( messages, Cmd.none )


isValid : Message -> Bool
isValid message =
    case message.value of
        InvalidMessage ->
            False

        _ ->
            True
