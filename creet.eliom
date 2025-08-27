open%client Eliom_content
open%client Html.D
open%client Js_of_ocaml
open%client Js_of_ocaml_lwt
open%client Lwt_js_events

type%client state =
  | Healthy
  | Sick
  | Berserk
  | Mean

type%client creet = {
  elt: Html_types.div elt;
  dom_elt: Dom_html.divElement Js.t;
  mutable is_alive: bool;
  mutable current_state: state;
  mutable size: float;
  mutable speed: float;
  mutable g_speed: float ref;
  mutable iter: int;
  mutable sick_count: int;

}

let%client default_creet_stats () =
  let size = 50. in
  let speed = 1. in
  let t_max = 700. -. size in
  let l_max = 1000. -. size in
  (speed, size, t_max, l_max)

let%client random_steps () =
  let step = max 0.25 (Random.float 0.75) in
  let t_step = step in
  let l_step = 1. -. step in
  ((if Random.bool () = true then t_step else Float.neg t_step),
   if Random.bool () = true then l_step else Float.neg l_step)

let%client init_creet () =
  let elt = div ~a:[ a_class [ "creet" ] ] [] in
  let speed, size, t_max, l_max = default_creet_stats () in
  let t_step, l_step = random_steps () in
  let creet = {
    elt;
  }


let%client create g_speed =
  let elt = div ~a:[ a_class [ "creet" ] ] [] in
  let speed
