module QuizView exposing (quizView)

import Html exposing (Html, text)
import Html.Attributes
import Html.Events exposing (onClick)
import List.Extra
import Model exposing (Model, Notable)
import Msg exposing (Msg)


quizView : Model -> List (Html Msg)
quizView model =
    case Model.nextNotable model of
        Just notable ->
            Html.h1 [] [ text "Quiz" ]
                :: (if completedNotableCount model <= Model.roundCompleteCount model then
                        if userGuessedNotable notable model then
                            [ waitingForOthersToGuessView ]

                        else
                            [ Html.h2 [] [ text "Who wrote this?" ]
                            , Html.p [] [ text notable.value ]
                            , guessView model notable
                            ]

                    else
                        [ guessingOverView notable model ]
                   )

        Nothing ->
            [ Html.h1 [] [ text "Quiz Done" ] ]


guessView : Model -> Notable -> Html Msg
guessView model notable =
    if model.userName /= notable.owner then
        let
            radioButtons =
                model
                    |> Model.getUsers
                    |> List.filter (\u -> u.name /= model.userName)
                    |> List.map
                        (\u ->
                            Html.div []
                                [ Html.input
                                    [ Html.Attributes.type_
                                        "radio"
                                    , Html.Attributes.name "guess"
                                    , Html.Attributes.value u.name
                                    , onClick (Msg.GuessUser u.name)
                                    ]
                                    []
                                , Html.label [ Html.Attributes.for u.name ] [ text u.name ]
                                ]
                        )
        in
        Html.form [ Html.Events.onSubmit Msg.SubmitGuess ]
            (radioButtons
                ++ [ Html.input
                        [ Html.Attributes.type_ "submit"
                        , Html.Attributes.value "Submit"
                        ]
                        []
                   ]
            )

    else
        Html.h3 [] [ text "Wait for someone to guess your thing..." ]


userGuessedNotable : Notable -> Model -> Bool
userGuessedNotable notable model =
    model
        |> Model.getGuesses
        |> List.any (\guess -> guess.guesser == model.userName && guess.notable == notable.value)


completedNotableCount : Model -> Int
completedNotableCount model =
    model
        |> Model.getNotables
        |> List.Extra.count (\notable -> Model.guessingOver notable model)


guessingOverView : Notable -> Model -> Html Msg
guessingOverView notable model =
    let
        outcomeText =
            case Model.favoriteGuess notable model of
                Just favoriteGuess ->
                    "Most people guessed that \""
                        ++ notable.value
                        ++ "\" belongs to "
                        ++ favoriteGuess.guess

                Nothing ->
                    "No guesses"
    in
    Html.h2 []
        [ text outcomeText
        ]


waitingForOthersToGuessView : Html Msg
waitingForOthersToGuessView =
    Html.div []
        [ Html.h2 [] [ text "Waiting for every to finish guessing..." ]
        ]
