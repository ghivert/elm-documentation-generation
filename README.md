# Elm Documentation Generation

This tool allows you to generate your documentation from your project. To do it, just run `elm make src/Main.elm --output dist/app.js && node src/Main.js` and you're good to go! All generated docs go into `docs/` folder.

# This tool is written entirely in Elm!

Yep, and that's why it's cool! The goal is to provide a full working documentation generator for your Elm code, written in Elm, so maybe pushed to Elm Reactor one day.  
Right now, it compiles to plain html files, and you don't need web unless you want nicer design.  
In the future, an Elm app will be built using the same code to serve the same informations!
