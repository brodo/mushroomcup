var elmDiv = document.getElementById('elm-main');
var elmApp = Elm.embed(Elm.Main, elmDiv, {"signUpResponsePort":{name: ""}});
elmApp.ports.signUpResponsePort.send({name: "test"});
