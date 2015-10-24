module.exports = function(grunt) {

  // Project configuration.
grunt.initConfig({
   elm: {
     compile: {
       files: {
         "js/elm.js": ["elm/Main.elm"]
       }
     }
   },
   watch: {
     elm: {
       files: ["elm/Main.elm"],
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
