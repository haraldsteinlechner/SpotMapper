module Main

open Feliz
open App
open Browser.Dom
open Elmish

open Feliz
open Elmish
open Elmish.React
open Elmish.HMR // Elmish.HMR needs to be the last open instruction in order to be able to shadow any supported API


//let root = ReactDOM.createRoot(document.getElementById "feliz-app")
//root.render(Components.Counter())

Program.mkProgram App.init update render
|> Program.withReactSynchronous "feliz-app"
|> Program.run   