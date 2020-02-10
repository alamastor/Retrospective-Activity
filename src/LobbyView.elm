module LobbyView exposing (lobbyView)

import Html exposing (Html, text)
import Model exposing (Model, User)
import Msg exposing (Msg)


lobbyView : Model -> List (Html Msg)
lobbyView model =
    [ Html.h2 [] [ text "Waiting for people to arrive..." ], Html.h2 [] [ text "Aready here:" ], usersView model ]


usersView : Model -> Html Msg
usersView model =
    Html.ul [] (List.map userView (Model.getUsers model))


userView : User -> Html Msg
userView user =
    Html.li [] [ Html.text user.name ]
