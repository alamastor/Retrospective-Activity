module Main exposing (..)

import Browser
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
    }


type alias User =
    { name : String
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


toUserName : Messages.Message -> Maybe String
toUserName message =
    case message.value of
        Messages.AddUser name ->
            Just name

        _ ->
            Nothing


toUser : String -> User
toUser name =
    { name = name }


getUsersReady : Model -> Bool
getUsersReady model =
    List.any isUsersReady model.messages


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


init : () -> ( Model, Cmd msg )
init _ =
    ( { messages = []
      , userInputVal = ""
      , userName = ""
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
    [ Html.h2 [] [ text "What happened this sprint??" ] ]


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


messagesView : Messages.Messages -> Html.Html Msg
messagesView messages =
    Html.div []
        [ Html.h2 [] [ text "Messages" ]
        , button
            [ onClick (Messages Messages.requestMessages) ]
            [ text "Get Messages" ]
        , Html.ul [] (getCurrentMessages messages |> List.map messageView)
        ]


messageView : Messages.Message -> Html.Html Msg
messageView message =
    Html.li []
        [ Html.text
            (Messages.messageTypeToString message.value ++ " - " ++ String.fromInt (Time.posixToMillis message.time))
        ]


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


mapToCmd : Cmd Messages.Msg -> Cmd Msg
mapToCmd cmd =
    Cmd.map (\mess -> Messages mess) cmd


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
