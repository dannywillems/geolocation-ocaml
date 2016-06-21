let doc = Dom_html.document

let data_to_str nb =
  match nb with
  | None -> "0"
  | Some x -> string_of_float x

let create_paragraph str =
  let p = Dom_html.createP doc in
  p##.innerHTML := Js.string str;
  Dom.appendChild doc##.body p

let on_device_ready () =
  let options =
    Cordova_geolocation.create_options ~maximum_age:1000 ()
  in
  let err_cb e =
    Jsoo_lib.alert (Cordova_geolocation.position_error_message e)
  in
  let id = Cordova_geolocation.watch_position
    (fun position ->
      let c = Cordova_geolocation.coords position in
      let time = Cordova_geolocation.timestamp position in
        create_paragraph
        (
          "Latitude: " ^
          (data_to_str (Cordova_geolocation.latitude c))
        );
        create_paragraph
        (
          "Longitude: " ^
          (data_to_str (Cordova_geolocation.longitude c))
        );
        create_paragraph
        (
          "Altitude: " ^
          (data_to_str (Cordova_geolocation.altitude c))
        );
        create_paragraph
        (
          "Accuracy: " ^
          (data_to_str (Cordova_geolocation.accuracy c))
        );
        create_paragraph
        (
          "Heading: " ^
          (data_to_str (Cordova_geolocation.heading c))
        );
        create_paragraph
        (
          "Time: " ^ (data_to_str (Some (float_of_int time)))
        );
      create_paragraph "------------------------"
    )
    err_cb
    ~options
    ()
  in
  let button = Dom_html.createButton doc in
  button##.innerHTML := Js.string "Stop";
  Lwt.async
  ( fun () ->
    Lwt_js_events.clicks button
    ( fun _ev _thread -> Cordova_geolocation.clear_watch id; Lwt.return ())
  );
  Dom.appendChild doc##.body button

let _ = Cordova.Event.device_ready on_device_ready
