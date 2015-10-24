var elmDiv = document.getElementById('elm-main');
var nullUser = {name: "", isLoggedIn: false};
var elmApp = Elm.embed(Elm.Main, elmDiv, {"signUpResponsePort":nullUser});
elmApp.ports.signUpResponsePort.send({name: "test", isLoggedIn: false});
elmApp.ports.signUpRequestPort.subscribe(function(request){
    console.log(request);
});
