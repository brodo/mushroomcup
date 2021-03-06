module.exports = function(grunt) {

  // Project configuration.
grunt.initConfig({
   elm: {
     compile: {
       files: {
         "js/elm.js": [
           "elm/MushroomCup.elm",
           "elm/Globals.elm",
           "elm/PlayerList.elm",
           "elm/Games.elm",
           "elm/ListUtils.elm"]
       }
     }
   },
   watch: {
     elm: {
       files: ["elm/MushroomCup.elm",
        "elm/Globals.elm",
        "elm/PlayerList.elm",
        "elm/Games.elm",
        "elm/ListUtils.elm"
      ],
       tasks: ["elm"]
     }
   },
   clean: ["elm-stuff/build-artifacts"]
  });

  grunt.loadNpmTasks('grunt-contrib-watch');
  grunt.loadNpmTasks('grunt-contrib-clean');
  grunt.loadNpmTasks('grunt-elm');

  grunt.registerTask('default', ['elm']);
};
