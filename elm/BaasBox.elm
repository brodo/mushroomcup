module BaasBox (signUp, signUpResponses, User) where


type alias UserRequest = {
  name : String
  ,password : String
}

type alias User = {
  name : String
}

port signUpResponsePort : Signal User

port signUpRequestPort : Signal UserRequest
port signUpRequestPort = signUpRequestMailbox.signal

signUpResponses : Signal User
signUpResponses = signUpResponsePort

signUpRequestMailbox : Signal.Mailbox UserRequest
signUpRequestMailbox = Signal.mailbox {name = "", password = ""}

signUp : UserRequest -> ()
signUp req =
  let
    _ = Signal.send signUpRequestMailbox.address req
  in
    ()
