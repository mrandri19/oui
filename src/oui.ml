open Html

module Make_App (App: sig
        type model
        val initialModel: model

        type action

        val update: model -> action -> model

        val render: model -> (action -> unit) -> vdom
    end)
= struct
    module D = Webapi.Dom

    (**
    * Converts a `Dom.text` node to a `Dom.node` node, created since
    * `Document.appendChild` doesn't let you append `Dom.text` nodes while the
    * javascript DOM API does.
    *)
    external __unsafe__textNode_to_node: Dom.text -> Dom.node = "%identity"
    (**
    * Converts a `Dom.element` node to a `Dom.node` node, created since
    * bs-webapi doesn't let you work with `Dom.element` nodes as if they
    * were nodes.
    *)
    external __unsafe__element_to_node: Dom.element -> Dom.node = "%identity"

    let initialModel = App.initialModel
    let render = App.render
    type action = App.action
    let update = App.update

    (**
    * Removes all children from the `root` node.
    *)
    let clear_root (root: Dom.node): unit =
        while (
            match D.Node.firstChild root with
            | Some child ->
                D.Node.removeChild child root |> ignore;
                true
            | None -> false
            )
        do () done

    let rec create_tree (root: Dom.node) (vdom: html): Dom.node = match vdom with
        | Element(name,attributes,handlers,children) ->
            let elem = D.Document.createElement name D.document in

            List.fold_left (fun el (a,b) -> ElementRe.setAttribute a b el; el) elem attributes |> ignore;
            List.fold_left (fun el (a,b) -> ElementRe.addEventListener a b el; el) elem handlers |> ignore;

            let children_trees = List.map (create_tree root) children in
            List.fold_left (fun el tree -> D.Element.appendChild tree el; el) elem children_trees
            |> __unsafe__element_to_node

        | Text(text) ->
            __unsafe__textNode_to_node (D.Document.createTextNode text D.document)

    let get_exn = function
    | Some x -> x
    | None   -> raise (Invalid_argument "Option.get")

    let contains needle haystack =
        try List.find (fun x -> x = needle) haystack; true
        with _ -> false

    (* https://reactjs.org/docs/reconciliation.html *)
    let rec reconcile (root: Dom.node) (old_vdom: vdom) (new_vdom: vdom): unit =
        match (old_vdom, new_vdom) with
        (* TODO: handle handlers *)
        (* The int_of_string error from get might be coming from here... *)
        | Element(n1,a1,_h1,c1),Element(n2,a2,_h2,c2) when n1=n2 ->
            let () = if a1 = a2
            then ()
            else
                (* Foreach of the attributes in a1 if it's not in a2 then remove it *)
                List.fold_left (
                    fun acc attr ->
                        if contains attr a2
                        then ()
                        else
                            D.Element.removeAttribute
                                (fst attr)
                                (ElementRe.ofNode root |> get_exn);
                            acc
                ) root a1 |> ignore;
                (* Foreach of the attributes in a2 if it's not in a1 then add it *)
                List.fold_left (
                    fun acc attr ->
                        if contains attr a1
                        then ()
                        else
                            D.Element.setAttribute
                                (fst attr)
                                (snd attr)
                                (ElementRe.ofNode root |> get_exn);
                            acc
                ) root a2 |> ignore;
            in
            (* If the lists have the same lenght simply iterate *)
            let () = if (List.length c1) = (List.length c2)
            then
                (List.fold_left2
                (fun acc a b ->
                    let child = D.NodeList.item acc (D.Node.childNodes root) |> get_exn
                    in
                    reconcile child a b;
                    acc+1
                )
                0
                c1
                c2
                |> ignore)
            else failwith "unimplemented with list with different length"
            in ()
        | Text(t1),Text(t2) when t1 = t2 -> ()
        | Text(_),Text(t2) -> D.Node.setTextContent root t2
        | _ ->
            clear_root root;
            let new_tree = create_tree root new_vdom in
            D.Node.appendChild new_tree root

    let update_dom (root: Dom.element) (old_vdom:vdom option) (new_vdom: vdom): unit =
        let root = __unsafe__element_to_node root in
        match old_vdom with
        | None ->
            clear_root root;
            let new_tree = create_tree root new_vdom in
            D.Node.appendChild new_tree root
        | Some(old_vdom) ->
            let elem = D.Node.firstChild root |> get_exn in
            reconcile elem old_vdom new_vdom

    let start_app (root: Dom.element) =
        let current_model = ref initialModel in
        let current_vdom: (Html.vdom option) ref = ref (None) in

        (* The function called every time an action is sent *)
        let rec tick action =
            Js.log "Tick";
            (* Update the model based on the action *)
            current_model := update !current_model action;
            Js.log !current_model;

            (* Render the next vdom based on the model *)
            let next_vdom = render !current_model tick in

            (* Update the DOM with the old vdom and the new on *)
            update_dom root !current_vdom next_vdom;

            (* Update the vdom *)
            current_vdom := Some next_vdom;
        in

        (* First render *)
        let next_vdom = render !current_model tick in
        update_dom root !current_vdom next_vdom;
        current_vdom := Some next_vdom;
end
