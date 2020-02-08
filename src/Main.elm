module Main exposing (main)

import Browser
import Dict
import Html exposing (Html, button, div, text)
import Html.Attributes
import Html.Events exposing (onClick)
import List.Extra
import Messages
import Random
import Random.List
import Set
import Time


admin : String
admin =
    "Alistair"


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { messages : Messages.Messages
    , userInputVal : String
    , userName : String
    , notableInputVal : String
    , messagesVisible : Bool
    , currentGuess : String
    }


type alias User =
    { name : String
    , notables : List String
    }


getScreen : Model -> Screen
getScreen model =
    if
        model.userName
            == ""
            || not (List.any (\user -> user.name == model.userName) (getUsers model))
    then
        Register

    else if
        List.length (getUsers model)
            > 0
            && allUsersGuessedTwice model
            && (List.length (getGuesses model) <= nextGuessCount model)
    then
        End

    else if List.length (getUsers model) > 0 && allNotablesComplete model then
        Quiz

    else if getUsersReady model then
        EnterNotable

    else
        Lobby


getUsers : Model -> List User
getUsers model =
    model.messages
        |> List.filterMap toUserName
        |> Set.fromList
        |> Set.toList
        |> List.map toUser
        |> List.map (addNotablesToUser (notableDict model.messages))


getSelf : Model -> User
getSelf model =
    model
        |> getUsers
        |> List.foldl (replaceIfSelf model) { name = model.userName, notables = [] }


replaceIfSelf : Model -> User -> User -> User
replaceIfSelf model new old =
    if new.name == model.userName then
        new

    else
        old


addNotablesToUser : Dict.Dict String (List String) -> User -> User
addNotablesToUser dict user =
    case Dict.get user.name dict of
        Just list ->
            { user | notables = list }

        Nothing ->
            user


notableDict : Messages.Messages -> Dict.Dict String (List String)
notableDict messages =
    notableDictInner messages Dict.empty


notableDictInner :
    Messages.Messages
    -> Dict.Dict String (List String)
    -> Dict.Dict String (List String)
notableDictInner messages dict =
    case messages of
        [] ->
            dict

        item :: rest ->
            case item.value of
                Messages.Notable userName thing ->
                    let
                        existing =
                            Dict.get userName dict
                    in
                    case existing of
                        Just list ->
                            notableDictInner rest (Dict.insert userName (thing :: list) dict)

                        Nothing ->
                            notableDictInner rest (Dict.insert userName [ thing ] dict)

                _ ->
                    notableDictInner rest dict


toUserName : Messages.Message -> Maybe String
toUserName message =
    case message.value of
        Messages.AddUser name ->
            Just name

        _ ->
            Nothing


toUser : String -> User
toUser name =
    { name = name, notables = [] }


getUsersReady : Model -> Bool
getUsersReady model =
    model.messages |> List.any isUsersReady


isUsersReady : Messages.Message -> Bool
isUsersReady message =
    case message.value of
        Messages.UsersReady ->
            True

        _ ->
            False


type Msg
    = Messages Messages.Msg
    | UserNameInput String
    | SubmitUsername
    | UsersReady
    | Start
    | NotableInput String
    | SubmitNotable
    | ToggleMessagesVisible
    | GuessUser String
    | SubmitGuess
    | NextGuess


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
    , body = adminPanel model :: screen model
    }


screen : Model -> List (Html Msg)
screen model =
    case getScreen model of
        Register ->
            register model

        Lobby ->
            lobby model

        EnterNotable ->
            enterNotable model

        Quiz ->
            quiz model

        End ->
            end model


end : Model -> List (Html Msg)
end model =
    [ Html.h2 [] [ text "Final Score" ]
    , Html.ul []
        (model
            |> getUsers
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
    if user.name == admin then
        7

    else
        getGuesses model
            |> List.filter (\guess -> guess.user == user.name)
            |> List.filter (guessCorrect model)
            |> List.length


guessCorrect : Model -> Guess -> Bool
guessCorrect model guess =
    let
        notable =
            getNotables model
                |> List.Extra.find (\n -> n.value == guess.notable)
    in
    case notable of
        Just n ->
            n.owner == guess.owner

        Nothing ->
            False


register : Model -> List (Html Msg)
register model =
    [ Html.h3 [] [ text "Please enter your name" ]
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


lobby : Model -> List (Html Msg)
lobby model =
    [ Html.h2 [] [ text "Waiting for people to arrive..." ], Html.h2 [] [ text "Aready here:" ], usersView model ]


enterNotable : Model -> List (Html Msg)
enterNotable model =
    [ selfNotablesView model
    , Html.h2 [] [ text "Enter something notable that happened this sprint:" ]
    ]
        ++ enterNotableInput model


enterNotableInput : Model -> List (Html Msg)
enterNotableInput model =
    if List.length (getSelf model).notables < 2 then
        [ Html.div []
            [ Html.textarea
                [ Html.Attributes.value model.notableInputVal
                , Html.Attributes.cols 50
                , Html.Attributes.rows 5
                , Html.Events.onInput NotableInput
                ]
                []
            ]
        , Html.button [ onClick SubmitNotable ] [ text "Submit" ]
        ]

    else
        [ Html.div [] [ text "Waiting for others to finish" ] ]


selfNotablesView : Model -> Html Msg
selfNotablesView model =
    Html.ul [] (getSelf model |> .notables |> List.map notableView)


notableView : String -> Html Msg
notableView notable =
    Html.li [] [ text notable ]


allNotablesComplete : Model -> Bool
allNotablesComplete model =
    List.all userNotablesComplete (getUsers model)


userNotablesComplete : User -> Bool
userNotablesComplete user =
    List.length user.notables >= 2


usersView : Model -> Html Msg
usersView model =
    Html.ul [] (List.map userView (getUsers model))


userView : User -> Html Msg
userView user =
    Html.li [] [ Html.text user.name ]


adminPanel : Model -> Html Msg
adminPanel model =
    if model.userName == admin then
        let
            panelChildren =
                [ Html.button [ onClick Start ] [ text "Restart" ]
                , Html.button [ onClick UsersReady ] [ text "Users Ready" ]
                , Html.button [ onClick NextGuess ] [ text "Next Guess" ]
                , Html.button [ onClick ToggleMessagesVisible ]
                    [ text
                        (if model.messagesVisible then
                            "Hide Messages"

                         else
                            "Show Messages"
                        )
                    ]
                ]
        in
        if model.messagesVisible then
            Html.div [] (panelChildren ++ [ messagesView model.messages ])

        else
            Html.div [] panelChildren

    else
        Html.div [] []


type Screen
    = Register
    | Lobby
    | EnterNotable
    | Quiz
    | End


messagesView : Messages.Messages -> Html.Html Msg
messagesView messages =
    Html.div []
        [ Html.h2 [] [ text "Messages" ]
        , button
            [ onClick (Messages Messages.GetMessages) ]
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


quiz : Model -> List (Html Msg)
quiz model =
    case nextGuess model of
        Just ( user, notable ) ->
            Html.h1 [] [ text "Quiz" ]
                :: (if List.length (getGuesses model) <= nextGuessCount model then
                        [ Html.h2 [] [ text (user.name ++ "'s turn to guess!") ]
                        , Html.h3 [] [ text "Who wrote this?" ]
                        , Html.p [] [ text notable.value ]
                        , guessView model user
                        ]

                    else
                        [ guessResultView model user notable ]
                   )

        Nothing ->
            [ Html.h1 [] [ text "Quiz Done" ] ]


guessResultView : Model -> User -> Notable -> Html Msg
guessResultView model user notable =
    let
        lastGuess =
            List.Extra.last (getGuesses model)
    in
    case lastGuess of
        Just guess ->
            if guess.owner == notable.owner then
                Html.div []
                    [ Html.h2 [] [ text "Correct!" ]
                    , Html.p []
                        [ text
                            (user.name
                                ++ " correctly guessed '"
                                ++ notable.value
                                ++ "' was written by "
                                ++ notable.owner
                            )
                        ]
                    ]

            else
                Html.div []
                    [ Html.h2 [] [ text "Incorrect!" ]
                    , Html.p []
                        [ text
                            (user.name
                                ++ " guessed '"
                                ++ notable.value
                                ++ "' was written by "
                                ++ guess.owner
                                ++ " but it was written by "
                                ++ notable.owner
                                ++ "."
                            )
                        ]
                    ]

        Nothing ->
            Html.div [] []


guessView : Model -> User -> Html Msg
guessView model user =
    if model.userName == user.name then
        let
            radioButtons =
                model
                    |> getUsers
                    |> List.filter (\u -> u.name /= user.name)
                    |> List.map
                        (\u ->
                            Html.div []
                                [ Html.input
                                    [ Html.Attributes.type_
                                        "radio"
                                    , Html.Attributes.name "guess"
                                    , Html.Attributes.value u.name
                                    , onClick (GuessUser u.name)
                                    ]
                                    []
                                , Html.label [ Html.Attributes.for u.name ] [ text u.name ]
                                ]
                        )
        in
        Html.form [ Html.Events.onSubmit SubmitGuess ]
            (radioButtons
                ++ [ Html.input
                        [ Html.Attributes.type_ "submit"
                        , Html.Attributes.value "Submit"
                        ]
                        []
                   ]
            )

    else
        Html.h3 [] [ text ("Waiting for " ++ user.name ++ " to guess...") ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Messages messagesMsg ->
            let
                ( m, c ) =
                    Messages.update messagesMsg model.messages
            in
            ( { model | messages = m }, mapToCmd c )

        UserNameInput name ->
            ( { model | userInputVal = name }, Cmd.none )

        Start ->
            ( model, Messages.start |> mapToCmd )

        UsersReady ->
            ( model, Messages.usersReady |> mapToCmd )

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

        NotableInput value ->
            ( { model | notableInputVal = value }, Cmd.none )

        SubmitNotable ->
            ( { model | notableInputVal = "" }
            , Messages.addNotable model.userName model.notableInputVal |> mapToCmd
            )

        ToggleMessagesVisible ->
            ( { model | messagesVisible = not model.messagesVisible }, Cmd.none )

        GuessUser user ->
            ( { model | currentGuess = user }, Cmd.none )

        SubmitGuess ->
            if model.currentGuess == "" then
                ( model, Cmd.none )

            else
                case nextGuess model of
                    Just ( user, notable ) ->
                        ( { model | currentGuess = "" }
                        , Messages.addGuess user.name notable.value model.currentGuess
                            |> mapToCmd
                        )

                    Nothing ->
                        ( model, Cmd.none )

        NextGuess ->
            if List.length (getGuesses model) > nextGuessCount model then
                ( model, Messages.addNextGuess |> mapToCmd )

            else
                ( model, Cmd.none )


mapToCmd : Cmd Messages.Msg -> Cmd Msg
mapToCmd cmd =
    Cmd.map (\mess -> Messages mess) cmd


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.map Messages (Time.every 1000 Messages.getMessagesTime)


nextGuessCount : Model -> Int
nextGuessCount model =
    model.messages
        |> List.filter isNextGuess
        |> List.length


isNextGuess : Messages.Message -> Bool
isNextGuess message =
    case message.value of
        Messages.NextGuess ->
            True

        _ ->
            False


userGuesses : User -> Model -> List Guess
userGuesses user model =
    getGuesses model
        |> List.filter (\guess -> guess.user == user.name)


getGuesses : Model -> List Guess
getGuesses model =
    List.filterMap messageToGuess model.messages


messageToGuess : Messages.Message -> Maybe Guess
messageToGuess message =
    case message.value of
        Messages.Guess user notable owner ->
            Just { user = user, notable = notable, owner = owner }

        _ ->
            Nothing


type alias Guess =
    { user : String
    , notable : String
    , owner : String
    }


guesserList : Model -> List User
guesserList model =
    let
        ( result, _ ) =
            Random.step
                (Random.List.shuffle (getUsers model ++ getUsers model))
                (Random.initialSeed 0)
    in
    result


guesserListWithNotables : Model -> List ( User, Notable )
guesserListWithNotables model =
    let
        ( result, _ ) =
            guesserListWithNotablesInner model (Random.initialSeed 0)
    in
    result


guesserListWithNotablesInner : Model -> Random.Seed -> ( List ( User, Notable ), Random.Seed )
guesserListWithNotablesInner model seed0 =
    let
        ( randomTry, seed1 ) =
            randomNotables model seed0

        try =
            List.Extra.zip (guesserList model) randomTry
    in
    if guesserListWithNotablesOk try then
        ( try, seed1 )

    else
        guesserListWithNotablesInner model seed1


guesserListWithNotablesOk : List ( User, Notable ) -> Bool
guesserListWithNotablesOk try =
    List.all (\( user, notable ) -> user.name /= notable.owner) try


nextGuess : Model -> Maybe ( User, Notable )
nextGuess model =
    model
        |> guesserListWithNotables
        |> List.drop (nextGuessCount model)
        |> List.head


randomNotables : Model -> Random.Seed -> ( List Notable, Random.Seed )
randomNotables model seed =
    Random.step (Random.List.shuffle (getNotables model)) seed


allUsersGuessedTwice : Model -> Bool
allUsersGuessedTwice model =
    List.all (\user -> List.length (userGuesses user model) > 1) (getUsers model)


type alias Notable =
    { value : String
    , owner : String
    }


getNotables : Model -> List Notable
getNotables model =
    List.filterMap toNotable model.messages


toNotable : Messages.Message -> Maybe Notable
toNotable message =
    case message.value of
        Messages.Notable user notable ->
            Just { value = notable, owner = user }

        _ ->
            Nothing
