module Main exposing (..)

import Browser
import Dict
import Html exposing (Html, button, div, text)
import Html.Attributes
import Html.Events exposing (onClick)
import Messages
import Set
import Time


admin =
    "Alistair"


main =
    Browser.document { init = init, view = view, update = update, subscriptions = subscriptions }


type alias Model =
    { messages : Messages.Messages
    , userInputVal : String
    , userName : String
    , notableInputVal : String
    }


type alias User =
    { name : String
    , notables : List String
    }


getCurrentMessages : Messages.Messages -> Messages.Messages
getCurrentMessages messages =
    getCurrentMessagesInner messages messages


getCurrentMessagesInner : Messages.Messages -> Messages.Messages -> Messages.Messages
getCurrentMessagesInner currentMessages unprocessedMessages =
    case unprocessedMessages of
        [] ->
            currentMessages

        message :: rest ->
            case message.value of
                Messages.Start ->
                    getCurrentMessagesInner (message :: rest) rest

                _ ->
                    getCurrentMessagesInner currentMessages rest


getScreen : Model -> Screen
getScreen model =
    if model.userName == "" then
        Register

    else if allNotablesComplete model then
        Quiz

    else if getUsersReady model then
        EnterNotable

    else
        Lobby


getUsers : Model -> List User
getUsers model =
    model.messages
        |> getCurrentMessages
        |> List.filterMap toUserName
        |> Set.fromList
        |> Set.toList
        |> List.map toUser
        |> List.map (addNotablesToUser (notableDict model.messages))


getSelf : Model -> User
getSelf model =
    model |> getUsers |> List.foldl (replaceIfSelf model) { name = model.userName, notables = [] }


replaceIfSelf : Model -> User -> User -> User
replaceIfSelf model new old =
    if new.name == model.userName then
        new

    else
        old


addNotablesToUser : Dict.Dict String (List String) -> User -> User
addNotablesToUser dict user =
    case Dict.get user.name dict of
        Just list ->
            { user | notables = list }

        Nothing ->
            user


notableDict : Messages.Messages -> Dict.Dict String (List String)
notableDict messages =
    notableDictInner messages Dict.empty


notableDictInner : Messages.Messages -> Dict.Dict String (List String) -> Dict.Dict String (List String)
notableDictInner messages dict =
    case messages of
        [] ->
            dict

        item :: rest ->
            case item.value of
                Messages.Notable userName thing ->
                    let
                        existing =
                            Dict.get userName dict
                    in
                    case existing of
                        Just list ->
                            notableDictInner rest (Dict.insert userName (thing :: list) dict)

                        Nothing ->
                            notableDictInner rest (Dict.insert userName [ thing ] dict)

                _ ->
                    notableDictInner rest dict


toUserName : Messages.Message -> Maybe String
toUserName message =
    case message.value of
        Messages.AddUser name ->
            Just name

        _ ->
            Nothing


toUser : String -> User
toUser name =
    { name = name, notables = [] }


getUsersReady : Model -> Bool
getUsersReady model =
    model.messages |> getCurrentMessages |> List.any isUsersReady


isUsersReady : Messages.Message -> Bool
isUsersReady message =
    case message.value of
        Messages.UsersReady ->
            True

        _ ->
            False


type Msg
    = Messages Messages.Msg
    | UserNameInput String
    | SubmitUsername
    | UsersReady
    | Start
    | NotableInput String
    | SubmitNotable


init : () -> ( Model, Cmd msg )
init _ =
    ( { messages = []
      , userInputVal = ""
      , userName = ""
      , notableInputVal = ""
      }
    , Cmd.none
    )


view : Model -> Browser.Document Msg
view model =
    { title = "Retrospective"
    , body = adminPanel model :: screen model
    }


screen : Model -> List (Html Msg)
screen model =
    case getScreen model of
        Register ->
            register model

        Lobby ->
            lobby model

        EnterNotable ->
            enterNotable model

        Quiz ->
            quiz model


register : Model -> List (Html Msg)
register model =
    [ Html.form [ Html.Events.onSubmit SubmitUsername ]
        [ Html.input
            [ Html.Attributes.type_ "text"
            , Html.Attributes.value model.userInputVal
            , Html.Events.onInput UserNameInput
            ]
            []
        , Html.input
            [ Html.Attributes.type_ "submit"
            , Html.Attributes.value "Submit"
            ]
            []
        ]
    ]


lobby : Model -> List (Html Msg)
lobby model =
    [ Html.h2 [] [ text "Waiting For Users" ], usersView model ]


enterNotable : Model -> List (Html Msg)
enterNotable model =
    [ Html.h2 [] [ text "What happened this sprint??" ]
    , selfNotablesView model
    ]
        ++ enterNotableInput model


enterNotableInput : Model -> List (Html Msg)
enterNotableInput model =
    if List.length (getSelf model).notables < 2 then
        [ Html.textarea
            [ Html.Attributes.value model.notableInputVal
            , Html.Events.onInput NotableInput
            ]
            []
        , Html.button [ onClick SubmitNotable ] [ text "Submit" ]
        ]

    else
        [ Html.div [] [ text "Waiting for others to finish" ] ]


selfNotablesView : Model -> Html Msg
selfNotablesView model =
    Html.ul [] (getSelf model |> .notables |> List.map notableView)


notableView : String -> Html Msg
notableView notable =
    Html.li [] [ text notable ]


allNotablesComplete : Model -> Bool
allNotablesComplete model =
    let
        userLength =
            Debug.log "userCount" (List.length (getUsers model))
    in
    List.all userNotablesComplete (getUsers model)


userNotablesComplete : User -> Bool
userNotablesComplete user =
    List.length user.notables >= 2


usersView : Model -> Html Msg
usersView model =
    Html.ul [] (List.map userView (getUsers model))


userView : User -> Html Msg
userView user =
    Html.li [] [ Html.text user.name ]


adminPanel : Model -> Html Msg
adminPanel model =
    if model.userName == admin then
        Html.div []
            [ Html.button [ onClick Start ] [ text "Restart" ]
            , Html.button [ onClick UsersReady ] [ text "Users Ready" ]
            , messagesView model.messages
            ]

    else
        Html.div [] []


type Screen
    = Register
    | Lobby
    | EnterNotable
    | Quiz


messagesView : Messages.Messages -> Html.Html Msg
messagesView messages =
    Html.div []
        [ Html.h2 [] [ text "Messages" ]
        , button
            [ onClick (Messages Messages.GetMessages) ]
            [ text "Get Messages" ]
        , Html.ul [] (getCurrentMessages messages |> List.map messageView)
        ]


messageView : Messages.Message -> Html.Html Msg
messageView message =
    Html.li []
        [ Html.text
            (Messages.messageTypeToString message.value ++ " - " ++ String.fromInt (Time.posixToMillis message.time))
        ]


quiz : Model -> List (Html Msg)
quiz model =
    [ Html.h1 [] [ text "Quiz" ] ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Messages messagesMsg ->
            let
                ( m, c ) =
                    Messages.update messagesMsg model.messages
            in
            ( { model | messages = m }, mapToCmd c )

        UserNameInput name ->
            ( { model | userInputVal = name }, Cmd.none )

        Start ->
            ( model, Messages.start |> mapToCmd )

        UsersReady ->
            ( model, Messages.usersReady |> mapToCmd )

        SubmitUsername ->
            if model.userInputVal == "" then
                ( model, Cmd.none )

            else
                ( { model
                    | userInputVal = ""
                    , userName = model.userInputVal
                  }
                , Messages.addUser model.userInputVal |> mapToCmd
                )

        NotableInput value ->
            ( { model | notableInputVal = value }, Cmd.none )

        SubmitNotable ->
            ( { model | notableInputVal = "" }, Messages.addNotable model.userName model.notableInputVal |> mapToCmd )


mapToCmd : Cmd Messages.Msg -> Cmd Msg
mapToCmd cmd =
    Cmd.map (\mess -> Messages mess) cmd


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map Messages (Time.every 1000 Messages.getMessagesTime)
