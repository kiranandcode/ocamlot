open Containers

module Middleware = struct
  let redirect_if_present var ~to_  : Dream.middleware =
    fun handler request -> 
      match Dream.session_field request var with
      | Some _ ->
        Dream.redirect request to_
      | None -> handler request

  let enforce_present_resource var : Dream.middleware =
    fun handler request -> 
    match Dream.session_field request var with
    | None -> Dream.respond ~status:`Unauthorized "unauthorized"
    | Some _ -> handler request

  let enforce_present var ~else_  : Dream.middleware =
    fun handler request -> 
    match Dream.session_field request var with
    | None -> Dream.redirect request else_
    | Some _ -> handler request

end

let sql req op =
  Dream.sql req op
  |> Lwt_result.map_error (fun err -> `DatabaseError (Caqti_error.show err))

let current_user req =
  match Dream.session_field req "user" with
  | None ->
    Lwt.return_ok None
  | Some username ->
    Dream.sql req @@ Database.LocalUser.find_user ~username
    |> Lwt_result.map_error (fun err -> `Internal ("Lookup user failed", Caqti_error.show err))

let current_user_link req =
  let open Lwt_result.Syntax in
  let* current_user = current_user req in
  match current_user with
  | None as opt -> Lwt.return_ok opt
  | Some user ->
    Dream.sql req (Database.Actor.create_local_user ~local_id:(user.Database.LocalUser.id))
    |> Lwt_result.map_error (fun err -> `Internal ("Lookup user failed", Caqti_error.show err))
    |> Lwt_result.map Option.some

let sanitize_form_error pp : 'a Dream.form_result Lwt.t -> _ Lwt_result.t =
  fun res ->
  Lwt.map (function
    | `Many_tokens data  -> Error (`FormError ("Many tokens", pp data))
    | `Missing_token data -> Error (`FormError ("Missing token", pp data))
    | `Invalid_token data -> Error (`FormError ("Wrong session", pp data))
    | `Wrong_session data  -> Error (`FormError ("Wrong session", pp data))
    | `Expired (data, _) -> Error (`FormError ("Expired form", pp data))
    | `Wrong_content_type -> Error (`FormError ("Wrong Content Type", "No further information"))
    | `Ok v -> Ok v
  ) res

let extract_single_multipart_data = function
  | [(None, data)] -> Ok data
  | _ -> Error ("expected a single text input")

let extract_file_multipart_data = function
  | [(Some fname, data)] -> Ok (fname, data)
  | _ -> Error ("expected a file input")

let extract_files_multipart_data data =
  List.map (function
    | (Some fname, data) -> Ok (fname, data)
    | _ -> Error ("expected a file input")) data
  |> List.all_ok

let mime_lookup filename =
  let content_type = Magic_mime.lookup filename in
  ["Content-Type", content_type]

let file ~local_root path =
  let file = Filename.concat local_root path in
  Lwt.catch
    (fun () ->
       Lwt_io.(with_file ~mode:Input file) (fun channel ->
         Lwt.bind (Lwt_io.read channel) @@ fun content ->
         Dream.response
           ~headers:(mime_lookup path) content
         |> Lwt.return_ok))
    (fun _exn ->
       Dream.response ~status:`Not_Found "Not found"
       |> Lwt.return_ok)

let respond ?status ?code ?headers text =
  Lwt.map Result.return @@ Dream.respond ?status ?code ?headers text

let redirect ?status ?code ?headers req path =
  Lwt.map Result.return @@ Dream.redirect ?status ?code ?headers req path

let activity_json  ?(status:Dream.status option) ?code ?(headers=[])  json =
  Lwt.map Result.return @@
  Dream.respond ?status ?code
    ~headers:(("Content-Type", Activitypub.Constants.ContentType.activity_json) :: headers)
    (Yojson.Safe.to_string json)

let json_pure ?(status:Dream.status option) ?code ?(headers=[]) json =
  Dream.respond ?status ?code ~headers:(("Content-Type", "application/json") :: headers)
    (Yojson.Safe.to_string json)

let json ?status ?code ?headers json =
  Lwt.map Result.return @@ json_pure ?status ?code ?headers json

let tyxml_gen k : ?status:Dream.status ->
  ?code:int ->
  ?headers:(string * string) list -> Tyxml_html.doc -> _ =
  let pp =
    Tyxml.Html.pp
      ~indent:false
      ~advert:{|
OCamlot
Copyright (C) 2022 The OCamlot Developers

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
|} () in
  fun ?(status: [< Dream.status] option) ?code ?headers res ->
    Format.ksprintf ~f:(fun res -> k @@ Dream.html ?status ?code ?headers res)
      "%a" pp res

let tyxml_pure ?status ?code ?headers doc =
  tyxml_gen Fun.id ?status ?code ?headers doc

let tyxml ?status ?code ?headers doc =
  tyxml_gen (Lwt.map Result.return) ?status ?code ?headers doc

let not_found_json ?headers msg =
  json ~status:`Not_Found ?headers (`Assoc [
    "type", `String "error";
    "reason", `String msg
  ])
  
