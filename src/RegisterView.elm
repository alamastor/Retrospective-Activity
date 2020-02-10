module RegisterView exposing (registerView)

import Html exposing (Html, text)
import Html.Attributes
import Html.Events
import Model exposing (Model)
import Msg exposing (Msg)


registerView : Model -> List (Html Msg)
registerView model =
    [ Html.h3 [] [ text "Please enter your name" ]
    , Html.form [ Html.Events.onSubmit Msg.SubmitUsername ]
        [ Html.input
            [ Html.Attributes.type_ "text"
            , Html.Attributes.value model.userInputVal
            , Html.Events.onInput Msg.UserNameInput
            ]
            []
        , Html.input
            [ Html.Attributes.type_ "submit"
            , Html.Attributes.value "Submit"
            ]
            []
        ]
    ]
