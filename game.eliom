open%shared Eliom_content.Html.D

let%shared elt = div ~a:[a_class ["game"]] []
let%shared creet_count_div = div ~a:[a_class ["creet-count"]] []

open%client Eliom_lib
open%client Eliom_content

(* open%client Js_of_ocaml *)
open%client Js_of_ocaml_lwt
open%client Creet

type%client game =
  { mutable iter : int
  ; mutable g_speed : float ref
  ; mutable creets : creet list
  ; mutable creet_count_span : Html_types.span elt }

let%client update_creets_counter g =
  let creets_count = List.length g.creets in
  let format = if creets_count = 1 then ' ' else 's' in
  let new_count =
    span [txt (Printf.sprintf "%d creets%c" creets_count format)]
  in
  let old_clount = g.creet_count_span in
  Html.Manip.replaceSelf old_clount new_count;
  g.creet_count_span <- new_count

let%client add_creets g =
  let creet = Creet.create g.g_speed in
  Html.Manip.appendChild ~%elt creet.elt;
  g.creets <- creet :: g.creets;
  update_creets_counter g

let%client get_available_creets creets =
  List.filter (fun c -> c.is_not_invulnerable) creets

let%client remove_creet g (creet : Creet.creet) =
  Html.Manip.removeSelf creet.elt;
  g.creets <- List.filter (fun c -> c != creet) g.creets;
  update_creets_counter g

let%client move_creet creets g creet =
  Lwt.async (fun () ->
    let creet_is_alive = Creet.move_creet creet creets in
    if not creet_is_alive then remove_creet g creet;
    Lwt.return ())

let%client rec spawn_game g =
  let%lwt () = Lwt_js.sleep 0.001 in
  let healthy, sick = List.partition (fun x -> x.state = Healthy) g.creets in
  if List.length healthy = 0
  then (alert "Game Over"; Lwt.return ())
  else (
    g.g_speed := !(g.g_speed) +. 0.0001;
    g.iter <- g.iter + 1;
    if g.iter = 3000
    then (
      add_creets g;
      g.iter <- 0);
    let creets =
      {healthy = get_available_creets healthy; sick = get_available_creets sick}
    in
    List.iter (move_creet creets g) creets.healthy;
    List.iter (move_creet creets g) creets.sick;
    spawn_game g)

let%client start_game () =
  Random.self_init ();
  let creet_count_span = span [txt "0 creets"] in
  let g : game = {iter = 0; g_speed = ref 0.; creets = []; creet_count_span} in
  Html.Manip.appendChild ~%creet_count_div creet_count_span;
  for _ = 1 to 4 do
    add_creets g
  done;
  Lwt.async (fun () -> spawn_game g)
