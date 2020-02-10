module Msg exposing (Msg(..))

import Messages
import Model exposing (User)


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
    | RoundComplete
    | RemoveUser User
