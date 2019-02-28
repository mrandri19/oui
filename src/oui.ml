[%%debugger.chrome]

open Html

module Make_App (App : sig
  type model

  val initialModel : model

  type action

  val update : model -> action -> model

  val render : model -> (action -> unit) -> vdom
end) =
struct
  module D = Webapi.Dom

  (* Unwraps an option, raising invalid argument if None is passed *)
  let get_exn ?(msg = "Option.get") () =
    function Some x -> x | None -> raise (Invalid_argument msg)

  (* Converts a `Dom.text` node to a `Dom.node` node, created since
   * `Document.appendChild` doesn't let you append `Dom.text` nodes while the
   * javascript DOM API does.
   *)
  external __unsafe__textNode_to_node : Dom.text -> Dom.node = "%identity"

  (* Converts a `Dom.element` node to a `Dom.node` node, created since
   * bs-webapi doesn't let you work with `Dom.element` nodes as if they
   * were nodes.
   *)
  external __unsafe__element_to_node : Dom.element -> Dom.node = "%identity"

  let initialModel = App.initialModel
  let render = App.render
  let update = App.update

  (* Removes all children from the `root` node *)
  let clear_root (root : Dom.node) : unit =
    while
      match D.Node.firstChild root with
      | Some child ->
          D.Node.removeChild child root |> ignore ;
          true
      | None -> false
    do
      ()
    done

  (* Applies (adds) the attribute to a dom element `el`. E.g. adds placeholder="foo" *)
  let apply_attribute el attr =
    match attr with
    | Event (event, handler) -> ElementRe.addEventListener event handler el
    | Style (k, v) ->
        let style =
          HtmlElementRe.style (HtmlElementRe.ofElement el |> get_exn ())
        in
        CssStyleDeclarationRe.setProperty k v "" style
    | Prop (k, v) -> (
        let input = HtmlInputElementRe.ofElement el |> get_exn () in
        match k with
        | "value" -> HtmlInputElementRe.setValue input v
        | "checked" -> HtmlInputElementRe.setChecked input (bool_of_string v)
        | _ -> failwith "cannot add unknown prop" )
    | Attr (k, v) -> ElementRe.setAttribute k v el

  (* Creates a tree of dom nodes using the virtual dom description *)
  let rec create_tree (vdom : vdom) : Dom.node =
    match vdom with
    | Element (name, attributes, children) ->
        let elem = D.Document.createElement name D.document in
        List.fold_left
          (fun el attr -> apply_attribute el attr ; el)
          elem attributes
        |> ignore ;
        let children_trees = List.map create_tree children in
        List.fold_left
          (fun el tree ->
            D.Element.appendChild tree el ;
            el )
          elem children_trees
        |> __unsafe__element_to_node
    | Text text ->
        __unsafe__textNode_to_node (D.Document.createTextNode text D.document)

  (* Checks if the element is in the list. O(n) *)
  let contains needle haystack =
    try
      List.find (fun x -> x = needle) haystack ;
      true
    with _ -> false

  (* Compare two lists of attributes: compares the handlers by reference and the rest by value *)
  let attributes_equal a b =
    let fn = function Event (_, _) -> false | _ -> true in
    List.filter fn a = List.filter fn b
    && List.filter (fun x -> not (fn x)) a
       == List.filter (fun x -> not (fn x)) b

  (* Applies fn to all elements in b but not in a. O(n^2) *)
  let iter_b_minus_a a b fn =
    List.fold_left
      (fun () x ->
        if contains x a then () else fn x ;
        () )
      () b

  (* https://reactjs.org/docs/reconciliation.html *)
  let rec reconcile (root : Dom.node) (old_vdom : vdom) (new_vdom : vdom) :
      unit =
    match (old_vdom, new_vdom) with
    | ( Element (name, attributes, children)
      , Element (name', attributes', children') )
      when name = name' ->
        let () =
          (* If the attributes are equal then do nothing *)
          if attributes_equal attributes attributes' then ()
          else
            (* If the differ, reconcile then *)
            let root_elem = ElementRe.ofNode root |> get_exn () in
            (* Remove all of the attributes which aren't anymore in attributes *)
            iter_b_minus_a attributes' attributes (function
              | Attr (k, _) -> ElementRe.removeAttribute k root_elem
              | Prop (k, _) -> (
                match k with
                | "value" ->
                    let input =
                      HtmlInputElementRe.ofElement root_elem |> get_exn ()
                    in
                    HtmlInputElementRe.setValue input ""
                | "checked" -> ElementRe.setNodeValue root_elem Js.Null.empty
                | _ -> failwith "cannot remove unknown prop" )
              | Event (e, h) -> ElementRe.removeEventListener e h root_elem
              | Style (k, _v) ->
                let style = HtmlElementRe.style (HtmlElementRe.ofElement root_elem |> get_exn ()) in
                CssStyleDeclarationRe.removeProperty k style |> ignore
            )
            |> ignore ;
            (* Add all of the new attributes (i.e. the ones not in attributes) *)
            iter_b_minus_a attributes attributes' (fun attr ->
                apply_attribute root_elem attr )
            |> ignore
        in
        (* TODO: implement key-based list diffing *)
        (* If the lists have the same lenght simply iterate *)
        let () =
          match compare (List.length children) (List.length children') with
          (* old children is shorter than new children *)
          | -1 ->
              let min_len = List.length children in
              let children'_first, children'_excess =
                Belt.List.splitAt children' min_len |> get_exn ()
              in
              (* Perform reconcile on all of the old children *)
              let () =
                List.fold_left2
                  (fun acc a b ->
                    let child =
                      D.NodeList.item acc (D.Node.childNodes root)
                      |> get_exn ()
                    in
                    reconcile child a b ; acc + 1 )
                  0 children children'_first
                |> ignore
              in
              (* Then simply insert the new ones *)
              List.iter
                (fun child ->
                  let new_tree = create_tree child in
                  D.Node.appendChild new_tree root )
                children'_excess
          (* old children is longer than new children *)
          | 1 ->
              let excess_len = List.length children - List.length children' in
              let children_second =
                Belt.List.drop children excess_len |> get_exn ()
              in
              (* First remove the excess *)
              let () =
                for _ = 0 to excess_len - 1 do
                  match D.Node.firstChild root with
                  | Some child -> D.Node.removeChild child root |> ignore
                  | None -> ()
                done
              in
              assert (List.length children_second = List.length children') ;
              (* Then perform reconcile *)
              List.fold_left2
                (fun acc a b ->
                  let child =
                    D.NodeList.item acc (D.Node.childNodes root)
                    |> get_exn ?msg:(Some "Cannot") ()
                  in
                  reconcile child a b ; acc + 1 )
                0 children_second children'
              |> ignore
          (* same length *)
          | 0 ->
              List.fold_left2
                (fun acc a b ->
                  let child =
                    D.NodeList.item acc (D.Node.childNodes root) |> get_exn ()
                  in
                  reconcile child a b ; acc + 1 )
                0 children children'
              |> ignore
          | _ -> failwith "impossible"
        in
        ()
    | Text t1, Text t2 when t1 = t2 -> ()
    | Text _, Text t2 -> D.Node.setTextContent root t2
    | _ ->
        clear_root root ;
        let new_tree = create_tree new_vdom in
        D.Node.appendChild new_tree root

  let update_dom (root : Dom.element) (old_vdom : vdom option)
      (new_vdom : vdom) : unit =
    let root = __unsafe__element_to_node root in
    match old_vdom with
    | None ->
        clear_root root ;
        let new_tree = create_tree new_vdom in
        D.Node.appendChild new_tree root
    | Some old_vdom ->
        let elem = D.Node.firstChild root |> get_exn () in
        reconcile elem old_vdom new_vdom

  let start_app (root : Dom.element) =
    let current_model = ref initialModel in
    let current_vdom = ref None in
    (* The function called every time an action is sent *)
    let rec tick action =
      (* Update the model based on the action *)
      current_model := update !current_model action ;
      (* Render the next vdom based on the model *)
      let next_vdom = render !current_model tick in
      (* Update the DOM with the old vdom and the new on *)
      update_dom root !current_vdom next_vdom ;
      (* Update the vdom *)
      current_vdom := Some next_vdom
    in
    (* First render *)
    let next_vdom = render !current_model tick in
    update_dom root !current_vdom next_vdom ;
    current_vdom := Some next_vdom
end
