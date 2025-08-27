open Eliom_content
open Html.D


module H42N42_app = Eliom_registration.App (struct
  let application_name = "H42N42"
  let global_data_path = None
end)

let main_service =
  Eliom_service.create ~path:(Eliom_service.Path [])
  ~meth:(Eliom_service.Get Eliom_parameter.unit) ()


let page =
  body
  [
    div
      ~a: [ a_class [ "canvas" ] ]
      [
        div ~a: [ a_class [ "river" ] ] [];
        div ~a: [ a_class [ "hospital" ] ] [];
      ];
    div
      ~a: [ a_class [ "about" ] ]
      [
        p [txt "About: H42N42"]
      ]
  ]

let () =
  H42N42_app.register ~service:main_service (fun () () ->
   Lwt.return
    (Eliom_tools.D.html ~title:"h42n42" ~css:[ ["css"; "h42n42.css" ] ] page))

