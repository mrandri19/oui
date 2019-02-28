[%%debugger.chrome]

open Oui

module App = Make_App (struct
  open Html

  type model = NotSent

  let initialModel = NotSent

  type action = Send of send

  and send = action -> unit

  let update model action =
    match action with
    | Send dispatch ->
      model

  let string_of_model = function NotSent -> "not sent"

  let render model dispatch =
    div []
      [ h1 [] [text "Hello"]
      ; div [Style ("display", "block")] [text (string_of_model model)]
      ; button [onClick dispatch (Send dispatch)] [text "send"] ]
end)

let () =
  let root =
    match Webapi.Dom.(Document.getElementById "root" document) with
    | Some a -> a
    | None -> failwith "cannot find root element"
  in
  App.start_app root
