type name = string
type attr = string * string
type handler = string * (Dom.event -> unit)

type html = Node of name * attr list * handler list * html list | Text of string

type vdom = html
