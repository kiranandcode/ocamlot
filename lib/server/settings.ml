[@@@warning "-33-32"]
open Containers
open Common

let log = Logging.add_logger "web.settings"

let handle_settings_get ?(errors=[]) req =
  let* current_user = Web.current_user req in
  match current_user with
  | Some user when user.is_admin ->
    let token = Dream.csrf_token req in 
    let* headers,action = Navigation.build_navigation_bar req in
    let title = "OCamlot settings page" in
    let* registration_allowed = Web.sql req (Database.Admin.is_registration_allowed) in
    Web.tyxml @@ View.Page.render_page title @@ (List.concat [
      [View.Header.render_header ?action headers;
       View.Components.render_heading ~icon:"S" ~current:"Settings" ~actions:[
          { text="Save"; url="save"; form=Some "settings-form" }
        ] ()];
      (match errors with
         [] -> []
       | errors -> [View.Error.render_error_list errors]);
      [View.Settings.render_settings_box ~fields:["dream.csrf", token] ~registration_allowed ()]
    ])
  | _ ->
    Web.redirect req "/feed"
    
let handle_settings_post req =
  let* current_user = Web.current_user req in
  match current_user with
  | Some user when user.is_admin ->
    let* data = Dream.form req |> Web.sanitize_form_error ([%show: (string * string) list]) in
    log.debug (fun f -> f "got post to settings page with data %s" ([%show: (string * string) list] data));
    let res =
      let open VResult in
      let* registration_allowed = form_data_present "registration-allowed" data |> Result.map_err List.return in
      Ok (registration_allowed) in
    begin match res with
    | Error errors ->
      handle_settings_get ~errors req
    | Ok registration_allowed ->
      let* _ = Web.sql req (Database.Admin.set_registration_allowed registration_allowed) in
      Web.redirect req "/settings"
    end
  | _ ->
    Web.redirect req "/feed"

let route =
  Dream.scope "/settings" [] [
    Dream.get "" @@ Error_display.handle_error_html @@ handle_settings_get;
    Dream.post "" @@ Error_display.handle_error_html @@ handle_settings_post;
  ]



