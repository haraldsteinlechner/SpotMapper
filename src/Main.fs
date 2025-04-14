module Main

open Elmish
open Elmish.HMR 

open SpotMapper

Program.mkProgram App.init App.update App.render
|> Program.withReactSynchronous "feliz-app"
|> Program.run   