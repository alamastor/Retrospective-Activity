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
    { messages : Messages.Messages, screen : Screen, users : List String, userInputVal : String }


processMessages : Model -> Model
processMessages model =
    model


processMessage : Model -> Messages.Message -> Model
processMessage model message =
    model


type Msg
    = Messages Messages.Msg
    | UserNameInput String
    | SubmitUsername


init : () -> ( Model, Cmd msg )
init _ =
    ( { messages = Just [], screen = Register, users = [], userInputVal = "" }, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Retrospective"
    , body = body model
    }


body : Model -> List (Html Msg)
body model =
    case model.screen of
        Register ->
            [ button [ onClick (Messages Messages.startGame) ] [ text "Start" ]
            , button [ onClick (Messages Messages.requestMessages) ] [ text "Get Messages" ]
            , messageView model.messages
            , Html.form [ Html.Events.onSubmit SubmitUsername ]
                [ Html.input [ Html.Attributes.type_ "text", Html.Attributes.value model.userInputVal, Html.Events.onInput UserNameInput ] []
                , Html.input [ Html.Attributes.type_ "button" ] []
                ]
            ]

        Lobby ->
            []


usersView : Model -> Html Msg
usersView model =
    Html.ul [] (List.map userView model.users)


userView : String -> Html Msg
userView user =
    Html.li [] [ Html.text user ]


type Screen
    = Register
    | Lobby


messageView : Messages.Messages -> Html.Html Msg
messageView messages =
    case messages of
        Just messageValues ->
            Html.ul []
                (List.map
                    (\message ->
                        Html.li []
                            [ Html.text (message.value ++ " - " ++ String.fromInt (Time.posixToMillis message.time))
                            ]
                    )
                    messageValues
                )

        Nothing ->
            Html.div [] [ Html.text "Error getting messages" ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Messages messagesMsg ->
            let
                ( m, c ) =
                    Messages.update messagesMsg model.messages
            in
            ( processMessages { model | messages = m }, Cmd.map (\mess -> Messages mess) c )

        UserNameInput name ->
            ( { model | userInputVal = name }, Cmd.none )

        SubmitUsername ->
            ( { model | userInputVal = "" }, Messages.addUser model.userInputVal )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
