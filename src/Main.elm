module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes
import Html.Events exposing (onClick)
import Messages
import Time


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


processMessages : Model -> Model
processMessages model =
    procMessages model.messages model


procMessages : Messages.Messages -> Model -> Model
procMessages messages model =
    case messages of
        [] ->
            model

        message :: rest ->
            model
                |> processMessage message
                |> procMessages rest


getScreen : Model -> Screen
getScreen model =
    if model.userName == "" then
        Register

    else
        Lobby


getUsers : Model -> List User
getUsers model =
    []


processMessage : Messages.Message -> Model -> Model
processMessage message model =
    model


type Msg
    = Messages Messages.Msg
    | UserNameInput String
    | SubmitUsername


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
    , body = [ usersView model, screen model ]
    }


screen : Model -> Html Msg
screen model =
    case getScreen model of
        Register ->
            Html.div []
                [ messageView model.messages
                , Html.form [ Html.Events.onSubmit SubmitUsername ]
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

        Lobby ->
            Html.div [] []


usersView : Model -> Html Msg
usersView model =
    Html.ul [] (List.map userView (getUsers model))


userView : User -> Html Msg
userView user =
    Html.li [] [ Html.text user.name ]


type Screen
    = Register
    | Lobby


messageView : Messages.Messages -> Html.Html Msg
messageView messages =
    Html.div []
        [ button [ onClick (Messages Messages.requestMessages) ] [ text "Get Messages" ]
        , Html.ul []
            (List.map
                (\message ->
                    Html.li []
                        [ Html.text
                            (Messages.messageTypeToString message.value ++ " - " ++ String.fromInt (Time.posixToMillis message.time))
                        ]
                )
                messages
            )
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Messages messagesMsg ->
            let
                ( m, c ) =
                    Messages.update messagesMsg model.messages
            in
            ( processMessages { model | messages = m }, mapToCmd c )

        UserNameInput name ->
            ( { model | userInputVal = name }, Cmd.none )

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
