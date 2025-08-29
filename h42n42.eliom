module Game = Game
module Creet = Creet
module%shared About = About
open Eliom_content.Html.D

let main_service =
  Eliom_service.create ~path:(Eliom_service.Path [])
    ~meth:(Eliom_service.Get Eliom_parameter.unit) ()

let page =
  body
    [ div
        ~a:[a_class ["canvas"]]
        [div ~a:[a_class ["river"]] []; div ~a:[a_class ["hospital"]] []]
    ; About.elm ]

module H42N42_app = Eliom_registration.App (struct
    let application_name = "H42N42"
    let global_data_path = None
  end)

let () =
  H42N42_app.register ~service:main_service (fun () () ->
    let _ = [%client (Game.start_game () : unit)] in
    Lwt.return
      (Eliom_tools.D.html ~title:"h42n42" ~css:[["css"; "h42n42.css"]] page))
