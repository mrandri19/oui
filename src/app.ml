open Oui
open Html
open Webapi.Dom

module App = Make_App(struct
    type model = int
    let initialModel = 0

    type action = Increase | Decrease | Set of int | Reset

    let update (model: model) (action: action): model =
        match action with
        | Increase -> model + 1
        | Decrease -> model - 1
        | Set value -> value
        | Reset -> 0

    let render (model: model) (send: action -> unit): vdom =
        Node("div",[],[],[
            Text(string_of_int model);
            Node("button", [], [("click", fun _e -> send Increase;)], [
                Text("increase")
            ]);
            Node("button", [],[("click", fun _e -> send Decrease)], [
                Text("decrease")
            ]);
            Node("button",[],[("click", fun _e -> send (Set 10))],[
                Text("Set to 10")
            ]);
            Node("button",[],[("click", fun _e -> send (Reset))],[
                Text("Reset")
            ]);
        ])
end)


let () =
    let root = match Document.getElementById "root" document with
    | Some(a) -> a
    | None -> failwith "cannot find root element"
    in
    App.start_app root
