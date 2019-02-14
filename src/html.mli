(**
* HTML element name
*)
type name = string

(**
* HTML element attribute
* e.g. ("display","none") will become display="none"
*)
type attr = string * string

(**
* Declare an handler for an event
* e.g. ("click", fun _e -> Js.log("Hello"))
*)
type handler = string * (Dom.event -> unit)

(**
* HTML AST
*)
type html =
| Element of name * attr list * handler list * html list
| Text of string

type vdom = html
