open%shared Eliom_content.Html.D

let%shared elt = div ~a:[ a_class [ "game" ] ] []
let%shared creet_count_div = div ~a:[ a_class [ "creet-count" ] ] []

open%client Eliom_lib
open%client Eliom_content
open%client Js_of_ocaml
open%client Js_of_ocaml_lwt
open%client Creet

type%client game = {
  mutable iter: int;
  mutable speed: float ref;
  mutable creets: creet list
}


let%client start_game () =
  Random.self_init ();
  let creet_count_span = span [txt "0 creets" ] in
  let g: game = {iter = 0; speed = ref 0} in
  Html.Manip.appendChild ~%creet_count_div creet_count_span;
  Lwt.async (fun () -> ())
