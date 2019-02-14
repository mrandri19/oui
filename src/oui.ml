open Html

module Make_App (App: sig
        type model
        val initialModel: model

        type action

        val update: model -> action -> model

        val render: model -> (action -> unit) -> vdom
    end)
= struct
    open Webapi.Dom
    type dom = string
    (**
    * Converts a `Dom.text` node to a `Dom.element` node, created since
    * `Document.appendChild` doesn't let you append `Dom.text` nodes while the
    * javascript DOM API does.
    *)
    external __unsafe__textNode_to_element: Dom.text -> Dom.element = "%identity"

    let initialModel = App.initialModel
    let render = App.render
    type action = App.action
    let update = App.update

    (**
    * Removes all children from the `root` element.
    *)
    let clear_root (root: Dom.element): unit =
        while (
            match Element.firstChild root with
            | Some child ->
                Element.removeChild child root |> ignore;
                true
            | None -> false
            )
        do () done

    (* TODO: optimize by diffing the doms *)
    let update_dom (root: Dom.element) ?(_old_vdom:vdom option=None) (new_vdom: vdom): unit =
        let rec create_tree root vdom = match vdom with
        | Node(name,attributes,handlers,children) ->
            let elem = Document.createElement name document in

            List.fold_left (fun el (a,b) -> Element.setAttribute a b el; el) elem attributes |> ignore;
            List.fold_left (fun el (a,b) -> Element.addEventListener a b el; el) elem handlers |> ignore;

            let children_trees = List.map (create_tree root) children in
            List.fold_left (fun el tree -> Element.appendChild tree el; el) elem children_trees

        | Text(text) ->
            __unsafe__textNode_to_element (Document.createTextNode text document)

        in
        clear_root root;
        let new_tree = create_tree root new_vdom in
        Element.appendChild new_tree root

    let start_app (root: Dom.element)  =
        let current_model = ref initialModel in
        let current_vdom tick = ref (render !current_model tick) in

        let rec tick action =
            current_model := update !current_model action;
            let next_vdom = render (!current_model) tick in
            update_dom root ~_old_vdom:(Some !(current_vdom tick)) next_vdom;
        in
        update_dom root !(current_vdom tick)
end
