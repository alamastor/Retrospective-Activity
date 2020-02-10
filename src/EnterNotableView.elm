module EnterNotableView exposing (enterNotableView)

import Html exposing (Html, text)
import Html.Attributes
import Html.Events exposing (onClick)
import Model exposing (Model)
import Msg exposing (Msg)


enterNotableView : Model -> List (Html Msg)
enterNotableView model =
    [ selfNotablesView model
    , Html.h2 [] [ text "Enter something notable that happened this sprint:" ]
    ]
        ++ enterNotableInput model


enterNotableInput : Model -> List (Html Msg)
enterNotableInput model =
    if List.length (Model.getSelf model).notables < Model.notablesPerUser then
        [ Html.div []
            [ Html.textarea
                [ Html.Attributes.value model.notableInputVal
                , Html.Attributes.cols 50
                , Html.Attributes.rows 5
                , Html.Events.onInput Msg.NotableInput
                ]
                []
            ]
        , Html.button [ onClick Msg.SubmitNotable ] [ text "Submit" ]
        ]

    else
        [ Html.div [] [ text "Waiting for others to finish" ] ]


selfNotablesView : Model -> Html Msg
selfNotablesView model =
    Html.ul [] (Model.getSelf model |> .notables |> List.map notableView)


notableView : String -> Html Msg
notableView notable =
    Html.li [] [ text notable ]
