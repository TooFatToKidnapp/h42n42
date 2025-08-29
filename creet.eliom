open%client Eliom_content
open%client Html.D
open%client Js_of_ocaml
open%client Js_of_ocaml_lwt
open%client Lwt_js_events

type%client state = Healthy | Sick | Berserk | Mean

type%client creet =
  { elt : Html_types.div elt
  ; dom_elt : Dom_html.divElement Js.t
  ; mutable is_not_invulnerable : bool
  ; mutable state : state
  ; mutable size : float
  ; mutable speed : float
  ; mutable g_speed : float ref
  ; mutable iter : int
  ; mutable sick_count : int
  ; max_direction_iter : int
  ; mutable top : float
  ; mutable t_min : float
  ; mutable t_max : float
  ; mutable t_step : float
  ; mutable left : float
  ; mutable l_min : float
  ; mutable l_max : float
  ; mutable l_step : float }

let%client default_creet_stats () =
  let size = 50. in
  let speed = 1. in
  let t_max = 1000. -. size in
  let l_max = 800. -. size in
  speed, size, t_max, l_max

let%client random_steps () =
  let step = max 0.25 (Random.float 0.75) in
  let t_step = step in
  let l_step = 1. -. step in
  ( (if Random.bool () = true then t_step else Float.neg t_step)
  , if Random.bool () = true then l_step else Float.neg l_step )

let%client get_creet_color state =
  Js.string
    (match state with
    | Healthy -> "dodgerblue"
    | Sick -> "darkblue"
    | Berserk -> "sienna"
    | Mean -> "tomato")

let%client to_px size = Js.string (Printf.sprintf "%fpx" size)

let%client init_creet g_speed =
  let elt = div ~a:[a_class ["creet"]] [] in
  let speed, size, t_max, l_max = default_creet_stats () in
  let t_step, l_step = random_steps () in
  let creet =
    { elt
    ; dom_elt = Html.To_dom.of_div elt
    ; is_not_invulnerable = true
    ; state = Healthy
    ; size
    ; speed
    ; g_speed
    ; iter = 0
    ; sick_count = 0
    ; max_direction_iter = 2500 + Random.int 1000
    ; top = max 65. (Random.float (t_max -. 15.))
    ; t_min = 0.
    ; t_max
    ; t_step
    ; left = max 15. (Random.float (l_max -. 15.))
    ; l_min = 0.
    ; l_max
    ; l_step }
  in
  creet.dom_elt##.style##.backgroundColor := get_creet_color creet.state;
  creet.dom_elt##.style##.height := to_px creet.size;
  creet.dom_elt##.style##.width := to_px creet.size;
  creet.dom_elt##.style##.top := to_px creet.top;
  creet.dom_elt##.style##.left := to_px creet.left;
  creet

let%client heal_creet creet =
  let speed, size, t_max, l_max = default_creet_stats () in
  creet.state <- Healthy;
  creet.speed <- speed;
  creet.size <- size;
  creet.t_max <- t_max;
  creet.l_max <- l_max;
  creet.sick_count <- 0

let%client mouse_event_handler creet event =
  let radius = creet.size /. 2. in
  let canvas = Dom_html.document##querySelector (Js.string ".canvas") in
  match Js.Opt.to_option canvas with
  | Some canvas_element ->
      let canvas_rect = canvas_element##getBoundingClientRect in
      let canvas_left = canvas_rect##.left in
      let canvas_top = canvas_rect##.top in
      let left = float_of_int event##.clientX -. canvas_left -. radius in
      let top = float_of_int event##.clientY -. canvas_top -. radius in
      creet.left <- max creet.l_min (min creet.l_max left);
      creet.top <- max creet.t_min (min creet.t_max top);
      creet.dom_elt##.style##.top := to_px creet.top;
      creet.dom_elt##.style##.left := to_px creet.left;
      let hospital_zone = creet.t_max -. 60. in
      if creet.state != Healthy && creet.top >= hospital_zone then (
        ignore (creet.dom_elt##.style##setProperty (Js.string "box-shadow") (Js.string "0 0 20px #4CAF50") Js.undefined);
        heal_creet creet;
        creet.dom_elt##.style##.backgroundColor := get_creet_color creet.state;
      ) else if creet.state != Healthy then (
        ignore (creet.dom_elt##.style##setProperty (Js.string "box-shadow") (Js.string "none") Js.undefined);
      )
  | None ->
      let left = float_of_int event##.clientX -. radius in
      let top = float_of_int event##.clientY -. radius in
      creet.left <- max creet.l_min (min creet.l_max left);
      creet.top <- max creet.t_min (min creet.t_max top);
      creet.dom_elt##.style##.top := to_px creet.top;
      creet.dom_elt##.style##.left := to_px creet.left

let%client handle_mouse_events creet mouse_event _ =
  creet.is_not_invulnerable <- false;
  creet.dom_elt##.style##.cursor := Js.string "grabbing";
  mouse_event_handler creet mouse_event;
  Lwt.pick
    [ mousemoves Dom_html.document (fun ev _ ->
        mouse_event_handler creet ev;
        Lwt.return ())
    ; (let%lwt mouse_up = mouseup Dom_html.document in
       mouse_event_handler creet mouse_up;
       creet.is_not_invulnerable <- true;
       creet.dom_elt##.style##.cursor := Js.string "grab";
       Lwt.return ()) ]

let%client create g_speed =
  let creet = init_creet g_speed in
  Lwt.async (fun () -> mousedowns creet.dom_elt (handle_mouse_events creet));
  creet

let%client get_position position step speed g_speed =
  position +. (step *. (speed +. !g_speed))

let%client get_random_steps () =
  let step = max 0.25 (Random.float 0.75) in
  let t_step = step in
  let l_step = 1. -. step in
  ( (if Random.bool () = true then t_step else Float.neg t_step)
  , if Random.bool () = true then l_step else Float.neg l_step )

let%client make_sick creet =
  let n = Random.int 100 in
  if n < 10
  then creet.state <- Berserk
  else if n >= 10 && n < 20
  then creet.state <- Mean
  else creet.state <- Sick;
  creet.dom_elt##.style##.backgroundColor := get_creet_color creet.state;
  creet.speed <- 0.85

let%client move creet =
  creet.top <- get_position creet.top creet.t_step creet.speed creet.g_speed;
  creet.left <- get_position creet.left creet.l_step creet.speed creet.g_speed;
  creet.dom_elt##.style##.top := to_px creet.top;
  creet.dom_elt##.style##.left := to_px creet.left

let%client get_distance_between_creets c1 c2 =
  let r1 = c1.size /. 2. in
  let x1 = c1.left +. r1 in
  let y1 = c1.top +. r1 in
  let r2 = c2.size /. 2. in
  let x2 = c2.left +. r2 in
  let y2 = c2.top +. r2 in
  Float.sqrt (((x1 -. x2) ** 2.) +. ((y1 -. y2) ** 2.))

let%client rec get_list_min ?(b_i = 1) ?(a_i = 0) a list list_length =
  if b_i < list_length
  then
    let b = List.nth list b_i in
    if b < a
    then get_list_min ~b_i:(b_i + 1) ~a_i:b_i b list list_length
    else get_list_min ~b_i:(b_i + 1) ~a_i a list list_length
  else a_i, a

let%client find_closest_creet c1 creets =
  let distances = List.map (get_distance_between_creets c1) creets in
  let first_distance = List.hd distances in
  let distances_length = List.length distances in
  let i, _ = get_list_min first_distance distances distances_length in
  List.nth creets i

let%client go_after_healthy_creet creet creets =
  let closest_healthy_creet = find_closest_creet creet creets in
  let top_diff = closest_healthy_creet.top -. creet.top in
  let left_diff = closest_healthy_creet.left -. creet.left in
  let total = Float.abs top_diff +. Float.abs left_diff in
  creet.t_step <- top_diff /. total;
  creet.l_step <- left_diff /. total

type%client creet_info = {healthy : creet list; sick : creet list}

let%client check_direction creet (creets : creet_info) =
  (* Check if creet is on the edge and should turn around *)
  if creet.top <= creet.t_min || creet.top >= creet.t_max
  then (
    if creet.top <= creet.t_min && creet.state = Healthy then make_sick creet;
    creet.t_step <- Float.neg creet.t_step;
    move creet)
  else if creet.left <= creet.l_min || creet.left >= creet.l_max
  then (
    creet.l_step <- Float.neg creet.l_step;
    move creet);
  if creet.state = Mean
  then (
    creet.iter <- 0;
    if creet.top -. 1. > creet.t_min && List.length creets.healthy > 0
    then go_after_healthy_creet creet creets.healthy)
  else if creet.iter = creet.max_direction_iter
  then (
    creet.iter <- 0;
    let t_step, l_step = get_random_steps () in
    creet.t_step <- t_step;
    creet.l_step <- l_step)

let%client creeds_intersect c1 c2 =
  let r1 = c1.size /. 2. in
  let r2 = c2.size /. 2. in
  let distance = get_distance_between_creets c1 c2 in
  distance <= r1 +. r2

let%client increase_creet_size creet =
  creet.size <- creet.size +. 0.05;
  creet.t_max <- creet.t_max -. 0.05;
  creet.l_max <- creet.l_max -. 0.05;
  creet.dom_elt##.style##.height := to_px creet.size;
  creet.dom_elt##.style##.width := to_px creet.size

let%client decrease_creet_size creet =
  creet.size <- creet.size -. 0.0025;
  creet.t_max <- creet.t_max +. 0.0025;
  creet.l_max <- creet.l_max +. 0.0025

let%client move_creet creet creets =
  creet.iter <- creet.iter + 1;
  check_direction creet creets;
  move creet;
  (* Return if creet is alive *)
  match creet.state with
  | Healthy ->
      let check_intersection sick =
        if creeds_intersect creet sick && Random.int 100 < 2
        then make_sick creet
      in
      List.iter check_intersection creets.sick;
      true
  | Sick ->
      creet.sick_count <- creet.sick_count + 1;
      creet.sick_count < 3000
  | Berserk ->
      if creet.size < 200. then (increase_creet_size creet; true) else false
  | Mean ->
      if creet.size > 42.5 then (decrease_creet_size creet; true) else false
