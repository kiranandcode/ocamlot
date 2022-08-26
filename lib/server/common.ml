open Containers
module APConstants = Activitypub.Constants

let (let+) x f = Lwt_result.bind x f
let (>>) x f = Lwt.map f x
let (>>=) x f = Lwt_result.bind x f
let map_err f x = Lwt_result.map_error f x
let return_error f v = Lwt.return (Result.map_err f v)
let return v = Lwt.return v
let return_ok v = Lwt.return_ok v
let lift_opt ~else_:else_ = function
  | None -> Error (else_ ())
  | Some v -> Ok v
let lift_pure res = Lwt.map Result.return res

module Middleware = struct
  let redirect_if_present var ~to_  : Dream.middleware =
    fun handler request -> 
      match Dream.session_field request var with
      | Some _ ->
        Dream.redirect request to_
      | None -> handler request


  let enforce_present var ~else_  : Dream.middleware =
    fun handler request -> 
    match Dream.session_field request var with
    | None -> Dream.redirect request else_
    | Some _ -> handler request

end

let current_user req =
  match Dream.session_field req "user" with
  | None ->
    return_ok None
  | Some username ->
    Dream.sql req @@ Database.LocalUser.lookup_user_exn ~username
    |> map_err (fun err -> `Internal ("Lookup user failed", err))
    |> Lwt_result.map Option.some

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
    ~indent:true
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
