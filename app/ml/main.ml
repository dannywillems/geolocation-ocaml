open Web
open Maps

let doc = Dom_html.document
let win = Dom_html.window

let my_position_img = "bluepoint.png"

let create_initial_map () =
  let width = Js.Optdef.get (win##.innerWidth) (fun () -> 300) in
  let width = (string_of_int width)^"px" in
  let div = Dom_html.createDiv doc in
  let () = div##.style##.width := Js.string width in
  let () = div##.style##.height := Js.string "500px" in
  let () = div##.id := Js.string "gmap" in
  Dom.appendChild doc##.body div

let click_event button funct =
  fun () -> Lwt_js_events.clicks button funct

let afficher_ma_position map _ _ =
  show_my_position map

let stop_ma_position map _ _ =
  hide_my_position ()

let start_tracking_p path _ _ =
  start_tracking path ()

let stop_tracking_p path _ _ =
  let l = stop_tracking path in
  let liste = coords_of_path path in
  let () = List.iter
      (fun (x,y) ->
         let str = Printf.sprintf "(%f,%f)" x y in
         Console.log str
      ) liste in
  l

type urls = string list
type names = string list

let (urls,names,coordinates) : urls * names * (float*float) list =
  ([ "http://static.fnac-static.com/multimedia/Images/FR/NR/91/54/65/6640785/1502-1.jpg";
     "http://www.famfamfam.com/lab/icons/silk/icons/disk.png";
     "http://www.famfamfam.com/lab/icons/silk/icons/magnifier.png"
    ],
    [ "Bob";
      "Alice";
      "John"
    ],
    [ (48.867979, 2.353407);
      (48.867436, 2.351079);
      (48.866074, 2.353193);
    ])

let spots_name,spots =
  [ "Parcours";
    "Skate";
    "Roller";
    "Autre";
    "Autre"
  ],
  [
    latlng_of_coords (48.867372, 2.353267);
    latlng_of_coords (48.863304, 2.343641);
    latlng_of_coords (48.849345, 2.337170);
    latlng_of_coords(48.869764, 2.349772);
    latlng_of_coords(48.867489, 2.349686)
  ]

let show_spots_fun map _ _ =
  let spots = add_marker_spots spots map in
  let _ = visible_zoom_level 12 ~markers:spots ~windows:[] map in
  Lwt.return ()

let show_friends_fun map _ _ =
  let windows = add_users_from_coords urls names coordinates map in
  Lwt.return ()

let test map =
  ()

let on_device_ready () =
  (** Creating initial map **)
  create_initial_map ();
  (** Create buttons **)
  let start_path = Dom_html.createButton doc in
  let stop_path  = Dom_html.createButton doc in
  let show_spots = Dom_html.createButton doc in
  let show_friends  = Dom_html.createButton doc in
  start_path##.innerHTML := Js.string "Start path";
  stop_path##.innerHTML := Js.string "Stop path";
  show_spots##.innerHTML := Js.string "Show spots";
  show_friends##.innerHTML := Js.string "Show friends";
  Dom.appendChild doc##.body start_path;
  Dom.appendChild doc##.body stop_path;
  Dom.appendChild doc##.body show_spots;
  Dom.appendChild doc##.body show_friends;
  (** My position button **)
  let ma_position = Dom_html.createButton doc in
  let stop_position = Dom_html.createButton doc in
  let () = ma_position##.innerHTML := Js.string "My position" in
  let () = stop_position##.innerHTML := Js.string "Stop position" in
  (** Adding the buttons to the view **)
  let () = Dom.appendChild doc##.body ma_position in
  let () = Dom.appendChild doc##.body stop_position in
  (** Adding map **)
  let centre = (49.2,2.0) in
  let map = create_map centre 10 "gmap" in
  let () = set_my_position_icon my_position_img in
  let () = test map in
  let path = create_path map in
  Lwt.async (click_event ma_position (afficher_ma_position map));
  Lwt.async (click_event stop_position (stop_ma_position map));
  Lwt.async (click_event start_path (start_tracking_p path));
  Lwt.async (click_event stop_path (stop_tracking_p path));
  Lwt.async (click_event show_spots (show_spots_fun map));
  Lwt.async (click_event show_friends (show_friends_fun map))

let _ = Cordova.Event.device_ready on_device_ready
