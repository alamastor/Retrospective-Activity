module EndView exposing (endView)

import Html exposing (Html, text)
import Model exposing (Model, User)
import Msg exposing (Msg)


endView : Model -> List (Html Msg)
endView model =
    [ Html.h2 [] [ text "Final Score" ]
    , Html.ul []
        (model
            |> Model.getUsers
            |> List.sortBy (\user -> userScore user model)
            |> List.reverse
            |> List.map
                (\user ->
                    Html.li []
                        [ text (user.name ++ ": " ++ String.fromInt (userScore user model))
                        ]
                )
        )
    ]


userScore : User -> Model -> Int
userScore user model =
    if user.name == Model.admin then
        7

    else
        Model.getGuesses model
            |> List.filter (\guess -> guess.guesser == user.name)
            |> List.filter (Model.guessCorrect model)
            |> List.length
