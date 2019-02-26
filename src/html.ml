(**
* HTML element name
*)
type name = string

(**
* Declare an handler for an event
* e.g. ("click", fun _e -> Js.log("Hello"))
*)
type handler = (Dom.event -> unit)

(**
* HTML element attribute
* e.g. ("display","none") will become display="none"
*)
type key = string
type value = string
type attribute =
 | Event of key * handler
 | Style of key * value
 | Prop of key * value
 | Attr of key * value

(**
* HTML AST
*)
type html =
| Element of name * attribute list * html list
| Text of string

type vdom = html

let div attributes children = Element("div",attributes,children)
let h1 attributes children = Element("h1",attributes,children)
let form attributes children = Element("form",attributes,children)
let input attributes children = Element("input",attributes,children)
let ul attributes children = Element("ul",attributes,children)
let li attributes children = Element("li",attributes,children)
let button attributes children = Element("button",attributes,children)
let br attributes children = Element("br",attributes,children)

let onClick send action = Event ("click", fun _e -> send action)

let text a = Text a
