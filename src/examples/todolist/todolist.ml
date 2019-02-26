open Oui

module App = Make_App (struct
  open Html

  type model = {todos: string list; form: string}

  let initialModel = {todos= []; form= ""}

  type action = AddTodo | RemoveTodo of string | UpdateText of string | Clear

  let update model (action : action) : model =
    let todos' =
      match action with
      | AddTodo -> model.form :: model.todos
      | RemoveTodo name -> List.filter (fun todo -> todo <> name) model.todos
      | _ -> model.todos
    in
    let form' =
      match action with
      | UpdateText text -> text
      | AddTodo -> ""
      | Clear -> ""
      | _ -> model.form
    in
    {todos= todos'; form= form'}

  let render model send =
    div []
      [ h1 [] [text "Hello"]
      ; text model.form
      ; button [Event ("click", fun _e -> send Clear)] [text "clear"]
      ; form
          [Event ("submit", fun e -> EventRe.preventDefault e ; send AddTodo)]
          [ input
              [ Attr ("type", "text")
              ; Prop ("value", model.form)
              ; Attr ("placeholder", "your todo text here")
              ; Event
                  ( "input"
                  , fun _event ->
                      send (UpdateText [%raw {|_event.target.value|}]) ) ]
              []
          ; button [Attr ("type", "submit")] [text "Add"] ]
      ; ul []
          (List.map
             (fun todo ->
               li []
                 [ Text todo
                 ; button
                     [Event ("click", fun _e -> send (RemoveTodo todo))]
                     [text "X"] ] )
             model.todos) ]
end)

let () =
  let root =
    match Webapi.Dom.(Document.getElementById "root" document) with
    | Some a -> a
    | None -> failwith "cannot find root element"
  in
  App.start_app root
