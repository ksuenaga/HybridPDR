[%%shared
    open Eliom_lib
    open Eliom_content
    open Html.D
]

module App_app =
  Eliom_registration.App (
    struct
      let application_name = "app"
      let global_data_path = None
    end)

let main_service =
  Eliom_service.create
    ~path:(Eliom_service.Path [])
    ~meth:(Eliom_service.Get Eliom_parameter.unit)
    ()

let () =
  App_app.register
    ~service:main_service
    (fun () () ->
      Lwt.return
        (Eliom_tools.D.html
           ~title:"HybridPDR"
           ~css:[["css";"app.css"]]
           Html.D.(body [
             h1 [pcdata "HybridPDR"];
           ])))
