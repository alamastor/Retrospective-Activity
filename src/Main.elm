module Main exposing (main)

import AdminView
import Browser
import EndView
import EnterNotableView
import Html exposing (Html)
import LobbyView
import Messages
import Model exposing (Model, User)
import Msg exposing (Msg)
import QuizView
import RegisterView
import Time


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


getScreen : Model -> Screen
getScreen model =
    if
        model.userName
            == ""
            || not (List.any (\user -> user.name == model.userName) (Model.getUsers model))
    then
        Register

    else if allNotablesComplete model && Model.guessingComplete model then
        End

    else if anyUsers model && allNotablesComplete model then
        Quiz

    else if usersReady model then
        EnterNotable

    else
        Lobby


anyUsers : Model -> Bool
anyUsers model =
    List.length (Model.getUsers model) > 0


usersReady : Model -> Bool
usersReady model =
    model.messages |> List.any isUsersReady


isUsersReady : Messages.Message -> Bool
isUsersReady message =
    case message.value of
        Messages.UsersReady ->
            True

        _ ->
            False


init : () -> ( Model, Cmd msg )
init _ =
    ( { messages = []
      , userInputVal = ""
      , userName = ""
      , notableInputVal = ""
      , messagesVisible = False
      , currentGuess = ""
      }
    , Cmd.none
    )


view : Model -> Browser.Document Msg
view model =
    { title = "Retrospective Activity"
    , body = AdminView.adminView model :: screen model
    }


screen : Model -> List (Html Msg)
screen model =
    case getScreen model of
        Register ->
            RegisterView.registerView model

        Lobby ->
            LobbyView.lobbyView model

        EnterNotable ->
            EnterNotableView.enterNotableView model

        Quiz ->
            QuizView.quizView model

        End ->
            EndView.endView model


allNotablesComplete : Model -> Bool
allNotablesComplete model =
    List.all userNotablesComplete (Model.getUsers model)


userNotablesComplete : User -> Bool
userNotablesComplete user =
    List.length user.notables >= Model.notablesPerUser


type Screen
    = Register
    | Lobby
    | EnterNotable
    | Quiz
    | End


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msg.Messages messagesMsg ->
            let
                ( m, c ) =
                    Messages.update messagesMsg model.messages
            in
            ( { model | messages = m }, mapToCmd c )

        Msg.UserNameInput name ->
            ( { model | userInputVal = name }, Cmd.none )

        Msg.Start ->
            ( model, Messages.start |> mapToCmd )

        Msg.UsersReady ->
            ( model, Messages.usersReady |> mapToCmd )

        Msg.SubmitUsername ->
            if model.userInputVal == "" then
                ( model, Cmd.none )

            else
                ( { model
                    | userInputVal = ""
                    , userName = model.userInputVal
                  }
                , Messages.addUser model.userInputVal |> mapToCmd
                )

        Msg.NotableInput value ->
            ( { model | notableInputVal = value }, Cmd.none )

        Msg.SubmitNotable ->
            ( { model | notableInputVal = "" }
            , Messages.addNotable model.userName model.notableInputVal |> mapToCmd
            )

        Msg.ToggleMessagesVisible ->
            ( { model | messagesVisible = not model.messagesVisible }, Cmd.none )

        Msg.GuessUser user ->
            ( { model | currentGuess = user }, Cmd.none )

        Msg.SubmitGuess ->
            if model.currentGuess == "" then
                ( model, Cmd.none )

            else
                case Model.nextNotable model of
                    Just notable ->
                        ( { model | currentGuess = "" }
                        , Messages.addGuess model.userName notable.value model.currentGuess
                            |> mapToCmd
                        )

                    Nothing ->
                        ( model, Cmd.none )

        Msg.RoundComplete ->
            if List.length (Model.getGuesses model) > Model.roundCompleteCount model then
                ( model, Messages.addNextGuess |> mapToCmd )

            else
                ( model, Cmd.none )

        Msg.RemoveUser user ->
            ( model, Messages.removeUser user.name |> mapToCmd )


mapToCmd : Cmd Messages.Msg -> Cmd Msg
mapToCmd cmd =
    Cmd.map (\mess -> Msg.Messages mess) cmd


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.map Msg.Messages (Time.every 1000 Messages.getMessagesTime)
