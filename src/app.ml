open Oui

module App = Make_App(struct
    open Html

    type model = int * string
    let initialModel = (0, "")

    type action = Increase | Decrease | Set of int | Reset | Input of string

    let update (counter, input) (action: action): model =
        match action with
        | Increase -> counter + 1, input
        | Decrease -> counter - 1, input
        | Set v -> v, input
        | Reset -> 0, input
        | Input input -> counter, input

    let render (counter, input) (send: action -> unit): vdom =
        Element("div",[],[],[
            Text(string_of_int counter);
            Element("br",[],[],[]);
            Element("button", [], [("click", fun _e -> send Increase;)], [
                Text("increase")
            ]);
            Element("button", [],[("click", fun _e -> send Decrease)], [
                Text("decrease")
            ]);
            Element("button",[],[("click", fun _e -> send (Reset))],[
                Text("Reset")
            ]);
            Element("button",[],[("click", fun _e -> Js.log input; send (Set (int_of_string input)))],[
                Text("Set")
            ]);
            Element(
                "input",
                [("type","number"); ("value",input); ("placeholder","your text here");("autofocus", "true")],
                [("input",fun _event ->
                    send (Input [%raw {|_event.target.value|}])
                )],
                []
            );
        ])
end)


let () =
    let root = match Webapi.Dom.(Document.getElementById "root" document) with
    | Some(a) -> a
    | None -> failwith "cannot find root element"
    in
    App.start_app root
