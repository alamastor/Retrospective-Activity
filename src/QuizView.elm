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
                            [ guessResultView model notable ]

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
        |> Debug.log "notables"
        |> List.Extra.count (\notable -> Model.guessingOver notable model)
        |> Debug.log "completedNotableCount"


guessingOverView : Notable -> Model -> Html Msg
guessingOverView notable model =
    case Model.correctGuesser notable model of
        Just guesser ->
            Html.h2 []
                [ text
                    (guesser
                        ++ " correctly guessed that\""
                        ++ notable.value
                        ++ "\" belongs to "
                        ++ notable.owner
                        ++ "!"
                    )
                ]

        Nothing ->
            Html.h2 []
                [ text
                    ("No one guessed that \""
                        ++ notable.value
                        ++ "\" belongs to "
                        ++ notable.owner
                        ++ "."
                    )
                ]


guessResultView : Model -> Notable -> Html Msg
guessResultView model notable =
    let
        lastGuess =
            List.Extra.last (Model.getGuesses model)
    in
    case lastGuess of
        Just guess ->
            if guess.guess == notable.owner then
                Html.div []
                    [ Html.h2 [] [ text "Correct!" ]
                    ]

            else
                Html.div []
                    [ Html.h2 [] [ text "Incorrect!" ]
                    ]

        Nothing ->
            Html.div [] []
