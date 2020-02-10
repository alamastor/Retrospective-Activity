module Model exposing
    ( Guess
    , Model
    , Notable
    , User
    , admin
    , correctGuesser
    , getGuesses
    , getNotables
    , getSelf
    , getUsers
    , guessCorrect
    , guessingComplete
    , guessingOver
    , nextNotable
    , notablesPerUser
    , roundCompleteCount
    )

import Dict
import List.Extra
import Messages
import Random
import Random.List
import Set exposing (Set)


admin : String
admin =
    "Alistair"


notablesPerUser : Int
notablesPerUser =
    1


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


type alias Notable =
    { value : String
    , owner : String
    }


type alias Guess =
    { guesser : String
    , notable : String
    , guess : String
    }


nextNotable : Model -> Maybe Notable
nextNotable model =
    model
        |> notables
        |> List.drop (roundCompleteCount model)
        |> List.head


roundCompleteCount : Model -> Int
roundCompleteCount model =
    model.messages
        |> List.filter isRoundComplete
        |> List.length


notables : Model -> List Notable
notables model =
    let
        ( return, _ ) =
            randomNotables model (Random.initialSeed 0)
    in
    return


isRoundComplete : Messages.Message -> Bool
isRoundComplete message =
    case message.value of
        Messages.RoundComplete ->
            True

        _ ->
            False


randomNotables : Model -> Random.Seed -> ( List Notable, Random.Seed )
randomNotables model seed =
    Random.step (Random.List.shuffle (getNotables model)) seed


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


guessingComplete : Model -> Bool
guessingComplete model =
    List.length (getUsers model)
        > 0
        && List.length (getNotables model)
        > 0
        && notablesGuessed model
        <= roundCompleteCount model
        && roundCompleteCount model
        >= List.length (getNotables model)


getUsers : Model -> List User
getUsers model =
    model.messages
        |> List.filterMap toUserName
        |> Set.fromList
        |> (\addedUsers -> Set.diff addedUsers (usersToRemove model))
        |> Set.toList
        |> List.map toUser
        |> List.map (addNotablesToUser (notableDict model.messages))


usersToRemove : Model -> Set String
usersToRemove model =
    model
        |> getRemovedUsers
        |> Set.filter (\user -> removeCountGEThanAddCount user model)


removeCountGEThanAddCount : String -> Model -> Bool
removeCountGEThanAddCount user model =
    (model.messages
        |> List.filterMap toRemovedUserName
        |> List.filter (\u -> u == user)
        |> List.length
    )
        >= (model.messages
                |> List.filterMap toUserName
                |> List.filter (\u -> u == user)
                |> List.length
           )


getRemovedUsers : Model -> Set String
getRemovedUsers model =
    model.messages
        |> List.filterMap toRemovedUserName
        |> Set.fromList


toRemovedUserName : Messages.Message -> Maybe String
toRemovedUserName message =
    case message.value of
        Messages.RemoveUser userName ->
            Just userName

        _ ->
            Nothing


userGuesses : User -> Model -> List Guess
userGuesses user model =
    getGuesses model
        |> List.filter (\guess -> guess.guesser == user.name)


getGuesses : Model -> List Guess
getGuesses model =
    List.filterMap messageToGuess model.messages


toUserName : Messages.Message -> Maybe String
toUserName message =
    case message.value of
        Messages.AddUser name ->
            Just name

        _ ->
            Nothing


messageToGuess : Messages.Message -> Maybe Guess
messageToGuess message =
    case message.value of
        Messages.Guess guesser notable guess ->
            Just { guesser = guesser, notable = notable, guess = guess }

        _ ->
            Nothing


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


toUser : String -> User
toUser name =
    { name = name, notables = [] }


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


guessCorrect : Model -> Guess -> Bool
guessCorrect model guess =
    let
        notable =
            getNotables model
                |> List.Extra.find (\n -> n.value == guess.notable)
    in
    case notable of
        Just n ->
            n.owner == guess.guess

        Nothing ->
            False


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


guessingOver : Notable -> Model -> Bool
guessingOver notable model =
    correctlyGuessed notable model || everyoneGuessed notable model


everyoneGuessed : Notable -> Model -> Bool
everyoneGuessed notable model =
    let
        guesserNames =
            Debug.log "guesserName"
                (model
                    |> notableGuesses notable
                    |> Debug.log "notableGuesses"
                    |> List.map (\guess -> guess.guesser)
                    |> Set.fromList
                    |> Set.insert notable.owner
                )

        userNames =
            Debug.log "userNames"
                (model
                    |> getUsers
                    |> List.map (\user -> user.name)
                    |> Set.fromList
                )
    in
    guesserNames == userNames


correctlyGuessed : Notable -> Model -> Bool
correctlyGuessed notable model =
    List.any (guessCorrect model) (notableGuesses notable model)


correctGuesser : Notable -> Model -> Maybe String
correctGuesser notable model =
    case List.filter (guessCorrect model) (notableGuesses notable model) of
        guess :: _ ->
            Just guess.guesser

        [] ->
            Nothing


notableGuesses : Notable -> Model -> List Guess
notableGuesses notable model =
    List.filter (\guess -> guessIsForNotable guess notable) (getGuesses model)


guessIsForNotable : Guess -> Notable -> Bool
guessIsForNotable guess notable =
    guess.notable == notable.value


notablesGuessed : Model -> Int
notablesGuessed model =
    model
        |> getNotables
        |> List.Extra.count (\notable -> guessingOver notable model)
