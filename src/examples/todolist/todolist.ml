[%%debugger.chrome]

open Oui

module App = Make_App (struct
  open Html

  type model = {todos: string list; form: string}

  let initialModel = {todos= []; form= ""}

  type action = AddTodo | RemoveTodo of string | UpdateText of string

  let update model (action : action) : model =
    let todos' =
      match action with
      | AddTodo -> model.form :: model.todos
      | RemoveTodo name ->
          List.filter (fun todo -> todo <> name) model.todos
      | _ -> model.todos
    in
    let form' =
      match action with
      | UpdateText text -> text
      | AddTodo -> ""
      | _ -> model.form
    in
    {todos= todos'; form= form'}

  let render model send =
    div []
      [ h1 [] [text "Hello"]
      ; form [onSubmit send AddTodo]
          [ input
              [ type_ "text"
              ; value model.form
              ; placeholder "your todo text here"
              ; onInput send (fun a -> UpdateText a) ]
              []
          ; button [type_ "submit"] [text "Add"] ]
      ; ul []
          (List.map
             (fun todo ->
               li []
                 [text todo; button [onClick send (RemoveTodo todo)] [text "X"]]
               )
             model.todos) ]
end)

let () =
  let root =
    match Webapi.Dom.(Document.getElementById "root" document) with
    | Some a -> a
    | None -> failwith "cannot find root element"
  in
  App.start_app root
