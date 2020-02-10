module AdminView exposing (adminView)

import Html exposing (Html, button, text)
import Html.Events exposing (onClick)
import List
import Messages
import Model exposing (Model, User)
import Msg exposing (Msg)
import Time


adminView : Model -> Html Msg
adminView model =
    if model.userName == Model.admin then
        let
            panelChildren =
                [ Html.button [ onClick Msg.Start ] [ text "Restart" ]
                , Html.button [ onClick Msg.UsersReady ] [ text "Users Ready" ]
                , Html.button [ onClick Msg.RoundComplete ] [ text "Round Complete" ]
                , Html.button [ onClick Msg.ToggleMessagesVisible ]
                    [ text
                        (if model.messagesVisible then
                            "Hide Messages"

                         else
                            "Show Messages"
                        )
                    ]
                , usersView model
                ]
        in
        if model.messagesVisible then
            Html.div [] (panelChildren ++ [ messagesView model.messages ])

        else
            Html.div [] panelChildren

    else
        Html.div [] []


messagesView : Messages.Messages -> Html.Html Msg
messagesView messages =
    Html.div []
        [ Html.h2 [] [ text "Messages" ]
        , button
            [ onClick (Msg.Messages Messages.GetMessages) ]
            [ text "Get Messages" ]
        , Html.ul [] (messages |> List.map messageView)
        ]


messageView : Messages.Message -> Html.Html Msg
messageView message =
    Html.li []
        [ Html.text
            (Messages.messageTypeToString message.value
                ++ " - "
                ++ String.fromInt (Time.posixToMillis message.time)
            )
        ]


usersView : Model -> Html.Html Msg
usersView model =
    Html.div []
        [ Html.h2 [] [ text "Users" ]
        , Html.ul [] (model |> Model.getUsers |> List.map userView)
        ]


userView : User -> Html.Html Msg
userView user =
    Html.li [] [ text user.name, button [ onClick (Msg.RemoveUser user) ] [ text "Remove user" ] ]
