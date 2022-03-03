open Containers
let (let+) x f = Lwt.bind x f


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

let is_not_empty opt = opt |> Option.filter (Fun.negate String.is_empty)

let get_errors request = 
  let error = Dream.session_field request "errors"
              |> is_not_empty in
  let+ () = Dream.set_session_field request "errors" "" in
  Lwt.return error

let set_error request error = Dream.set_session_field request "errors" error

let request_bind request result f =
  let+ result = result in 
  match result with
  | Error str ->
    let+ () = set_error request str in
    Dream.redirect request "/error"
  | Ok vl -> f vl

let with_current_user request f =
  let (let@) x f = request_bind request x f in
  match Dream.session_field request "user" with
  | Some username ->
    let@ user = Dream.sql request @@ Database.User.lookup_user_exn ~username in
    f (Some user)
  | None -> f None
