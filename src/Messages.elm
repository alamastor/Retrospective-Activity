module Messages exposing
    ( Message
    , MessageType(..)
    , Messages
    , Msg(..)
    , addGuess
    , addNextGuess
    , addNotable
    , addUser
    , getMessagesTime
    , messageTypeToString
    , prepareMessage
    , removeUser
    , start
    , update
    , usersReady
    )

import Http
import Json.Decode
import Json.Encode
import List.Extra
import Task
import Time


url : String
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
    | RemoveUser String
    | UsersReady
    | Notable String String
    | Guess String String String
    | RoundComplete


getMessagesTime : Time.Posix -> Msg
getMessagesTime _ =
    GetMessages


messageTypeToString : MessageType -> String
messageTypeToString messageType =
    case messageType of
        InvalidMessage ->
            "?"

        Start ->
            "START"

        AddUser user ->
            "ADD_USER " ++ unSpace user

        RemoveUser user ->
            "REMOVE_USER " ++ unSpace user

        UsersReady ->
            "USERS_READY"

        Notable user notable ->
            "NOTABLE " ++ unSpace user ++ " " ++ unSpace notable

        Guess user notable owner ->
            "GUESS " ++ unSpace user ++ " " ++ unSpace notable ++ " " ++ unSpace owner

        RoundComplete ->
            "ROUND_COMPLETE"


space : String -> String
space str =
    String.replace "_" " " str


unSpace : String -> String
unSpace str =
    String.replace " " "_" str


start : Cmd Msg
start =
    prepareMessage Start


addUser : String -> Cmd Msg
addUser userName =
    AddUser userName |> prepareMessage


removeUser : String -> Cmd Msg
removeUser user =
    RemoveUser user |> prepareMessage


usersReady : Cmd Msg
usersReady =
    prepareMessage UsersReady


addNotable : String -> String -> Cmd Msg
addNotable userName notable =
    prepareMessage (Notable userName notable)


addGuess : String -> String -> String -> Cmd Msg
addGuess userName notable owner =
    prepareMessage (Guess userName notable owner)


addNextGuess : Cmd Msg
addNextGuess =
    prepareMessage RoundComplete


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

                    userName :: _ ->
                        AddUser (space userName)

            else if msgType == "REMOVE_USER" then
                case values of
                    [] ->
                        InvalidMessage

                    userName :: _ ->
                        RemoveUser (space userName)

            else if msgType == "USERS_READY" then
                UsersReady

            else if msgType == "NOTABLE" then
                case values of
                    [] ->
                        InvalidMessage

                    userName :: stuff ->
                        case stuff of
                            [] ->
                                InvalidMessage

                            notable :: _ ->
                                Notable (space userName) (space notable)

            else if msgType == "GUESS" then
                case values of
                    [] ->
                        InvalidMessage

                    userName :: stuff ->
                        case stuff of
                            [] ->
                                InvalidMessage

                            notable :: more ->
                                case more of
                                    [] ->
                                        InvalidMessage

                                    owner :: _ ->
                                        Guess (space userName) (space notable) (space owner)

            else if msgType == "ROUND_COMPLETE" then
                RoundComplete

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
                    ( (messages ++ messageListOk) |> processMessages, Cmd.none )

                Err _ ->
                    ( messages, Cmd.none )

        SendMessage message ->
            ( (message :: messages) |> processMessages, postMessage message )

        Sent _ ->
            ( messages, Cmd.none )


processMessages : Messages -> Messages
processMessages messages =
    messages
        |> List.filter isValid
        |> List.Extra.uniqueBy
            (\message ->
                messageTypeToString message.value
                    ++ String.fromInt (Time.posixToMillis message.time)
            )
        |> List.sortBy (\message -> Time.posixToMillis message.time)
        |> getCurrentMessages


isValid : Message -> Bool
isValid message =
    case message.value of
        InvalidMessage ->
            False

        _ ->
            True


getCurrentMessages : Messages -> Messages
getCurrentMessages messages =
    getCurrentMessagesInner messages messages


getCurrentMessagesInner : Messages -> Messages -> Messages
getCurrentMessagesInner currentMessages unprocessedMessages =
    case unprocessedMessages of
        [] ->
            currentMessages

        message :: rest ->
            case message.value of
                Start ->
                    getCurrentMessagesInner (message :: rest) rest

                _ ->
                    getCurrentMessagesInner currentMessages rest
