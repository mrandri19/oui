open Html

module Make_App
    (App: sig
        type model
        val initialModel: model

        type action

        val update: model -> action -> model

        val render: model -> (action -> unit) -> vdom
    end)
: sig
    (**
    * Starts the application.
    *)
    val start_app: Dom.element -> unit
end
