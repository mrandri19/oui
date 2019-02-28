# Oui

Oui is an elm-like framework, based on the elm architechture, which I've built to understand the internals of a modern web framework and to have fun :D.

It is **NOT** production ready use [elm](https://elm-lang.org), [ReasonReact](https://reasonml.github.io/) or maybe [bucklescript-tea](https://github.com/OvermindDL1/bucklescript-tea/) if you want a production-ready solution.

## Sample code

This makes a simple counter with 4 buttons (Increase, Decrease, Reset and Set).

```ocaml
open Oui

module App = Make_App (struct
  open Html

  type model = int * string

  let initialModel = (0, "")

  type action = Increase | Decrease | Set | Reset | Input of string

  let update (counter, input) (action : action) : model =
    match action with
    | Increase -> (counter + 1, input)
    | Decrease -> (counter - 1, input)
    | Set -> (int_of_string input, "")
    | Reset -> (0, input)
    | Input input -> (counter, input)

  let render (counter, input_) (send : action -> unit) : vdom =
    div []
      [ h1 [] [text "Counter"]
      ; text (string_of_int counter)
      ; br [] []
      ; button [onClick send Increase] [text "increase"]
      ; button [onClick send Decrease] [text "decrease"]
      ; button [onClick send Reset] [text "Reset"]
      ; button [onClick send Set] [text "Set"]
      ; input
          [ Attr ("type", "number")
          ; Prop ("value", input_)
          ; Attr ("placeholder", "your text here")
          ; Event
              ( "input"
              , fun _event -> send (Input [%raw {|_event.target.value|}]) ) ]
          [] ]
end)

let () =
  let root =
    match Webapi.Dom.(Document.getElementById "root" document) with
    | Some a -> a
    | None -> failwith "cannot find root element"
  in
  App.start_app root
```
