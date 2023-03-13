open Containers
module APConstants = Activitypub.Constants


let (let*) x f = Lwt_result.bind x f
let (let+) x f = Lwt_result.map f x
let (>>) x f = Lwt.map f x
let (>>=) x f = Lwt_result.bind x f
let map_err f x = Lwt_result.map_error f x
let return_error f v = Lwt.return (Result.map_err f v)
let return v = Lwt.return v
let return_ok v = Lwt.return_ok v
let lift_opt ~else_:else_ = function
  | None -> Error (else_ ())
  | Some v -> Ok v
let get_opt ~else_ opt = Lwt.return (lift_opt ~else_ opt)
let lift_pure res = Lwt.map Result.return res
let form_data key form =
  List.Assoc.get ~eq:String.equal key form
  |> Option.to_result (Format.sprintf "missing field %s" key)

let form_data_present key form =
  List.Assoc.get ~eq:String.equal key form
  |> function None -> Ok false | Some _ -> Ok true


let map_list f ls =
  Lwt.map Result.flatten_l (Lwt_list.map_s f ls)

(* Result monad for validation *)
module VResult = struct
  include Result

  let lift err = Result.map_err List.return err

  let lift_pair_fst f (l,r) =
    match f l with
    | Ok l -> Ok (l,r)
    | Error _ as err -> err
  let lift_pair_snd f (l,r) =
    match f r with
    | Ok r -> Ok (l,r)
    | Error _ as err -> err

  let lift_result_check f = function
      None -> Ok None
    | Some data -> f data |> Result.map Option.some

  let (and*) x y = match x,y with
      Ok x, Ok y -> Ok (x,y)
    | Error l, Error r -> Error (l @ r)
    | Error e, _ | _, Error e -> Error e

  let check_input_size field max_size data =
    if String.length data > max_size
    then Error (
      `InputTooLarge (
        field,
        max_size,
        "[" ^
        string_of_int (String.length data) ^
        "]" ^
        String.take 100 data ^
        "..."
      ))
    else Ok data

  let ensure msg cond = if cond then Ok () else Error [msg]

end

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
  |> map_err (fun err -> `DatabaseError (Caqti_error.show err))

let current_user req =
  match Dream.session_field req "user" with
  | None ->
    return_ok None
  | Some username ->
    Dream.sql req @@ Database.LocalUser.find_user ~username
    |> map_err (fun err -> `Internal ("Lookup user failed", Caqti_error.show err))

let current_user_link req =
  let* current_user = current_user req in
  match current_user with
  | None as opt -> return_ok opt
  | Some user ->
    Dream.sql req (Database.Actor.lookup_local_user ~id:(user.Database.LocalUser.id))
    |> map_err (fun err -> `Internal ("Lookup user failed", Caqti_error.show err))
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
  lift_pure @@
  Dream.respond ?status ?code
    ~headers:(("Content-Type", Activitypub.Constants.ContentType.activity_json) :: headers)
    (Yojson.Safe.to_string json)

let json_pure ?(status:Dream.status option) ?code ?(headers=[]) json =
  Dream.respond ?status ?code ~headers:(("Content-Type", "application/json") :: headers)
    (Yojson.Safe.to_string json)

let json ?status ?code ?headers json =
  lift_pure @@ json_pure ?status ?code ?headers json

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
  tyxml_gen lift_pure ?status ?code ?headers doc

let not_found_json ?headers msg =
  json ~status:`Not_Found ?headers (`Assoc [
      "type", `String "error";
      "reason", `String msg
    ])
  
