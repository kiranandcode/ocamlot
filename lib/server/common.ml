open Containers

(*

('a -> Dream.response Lwt.t) -> Dream.response Lwt.t

('a -> 'b) -> 'a -> 'b

*)
let (let>) x f = x f
let (let+) x f = Lwt.bind x f
let (let+!) x f = Lwt_result.bind x f

let internal_error = Dream.respond ~status:`Internal_Server_Error "Internal server error"
let bad_request = Dream.respond ~status:`Bad_Request "Bad Request"
let not_acceptable = Dream.respond ~status:`Not_Acceptable "Bad Request"
let not_found ?(msg="Not found") () = Dream.respond ~status:`Not_Found msg

let activity_json json =
  Dream.respond ~headers:[("Content-Type", Activitypub.Constants.ContentType.activity_json)]
      (Yojson.Safe.to_string json)

let or_error ?(err=internal_error) ?req vl knt = 
  match vl with
  | Ok vl -> knt vl
  | Error e ->
    Dream.error (fun log -> log ?request:req "error: %s" e);
    err

let or_errorP ?(err=internal_error) ~req vl knt =
  Lwt.bind vl @@ function 
  | Ok vl -> knt vl
  | Error e ->
    Dream.error (fun log -> log ~request:req "error: %s" e);
    err

let or_else ~else_ vl knt = 
  match vl with
  | Some vl -> knt vl
  | _ -> else_

let or_not_acceptable ?msg vl knt =
  or_else ~else_:(not_found ?msg ()) vl knt

let or_not_found ?msg vl knt =
  or_else ~else_:(not_found ?msg ()) vl knt

let or_bad_reqeust vl knt =
  or_else ~else_:bad_request vl knt

let holds_or_bad_request vl knt =
  or_bad_reqeust (if vl then Some () else None) knt

let holds_or ~else_ vl knt =
  or_else ~else_ (if vl then Some () else None) knt

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

module Error = struct

  let get request = 
    let is_not_empty opt = opt |> Option.filter (Fun.negate String.is_empty) in
    let error = Dream.session_field request "errors"
                |> is_not_empty in
    let+ () = Dream.set_session_field request "errors" "" in
    Lwt.return error

  let set request error = Dream.set_session_field request "errors" error

end

let request_bind request result f =
  let+ result = result in 
  match result with
  | Error str ->
    let+ () = Error.set request str in
    Dream.redirect request "/error"
  | Ok vl -> f vl

let with_current_user request f =
  let (let@) x f = request_bind request x f in
  match Dream.session_field request "user" with
  | Some username ->
    let@ user = Dream.sql request @@ Database.LocalUser.lookup_user_exn ~username in
    f (Some user)
  | None -> f None

let with_param param f req ~then_ ~else_ =
  let (let+) x f = request_bind req x f in
  let param = Dream.param req param in
  let+ res = f req param in
  match res with
  | None -> else_ ()
  | Some res -> then_ res
  
