var elmDiv = document.getElementById('elm-main');
var elmApp = Elm.embed(Elm.MushroomCup, elmDiv);
elmApp.ports.focus.subscribe(function(selector){
  setTimeout(function() {
        var nodes = document.querySelectorAll(selector);
        if (nodes.length === 1 && document.activeElement !== nodes[0]) {
            nodes[0].focus()
        }
    }, 50);
});
