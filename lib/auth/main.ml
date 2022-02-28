[@@@warning "-27"]
type yojson = Yojson.Safe.t [@@deriving show]
let yojson_of_yojson = Fun.id

open Containers

module SMap = struct
  include (Map.Make (String))

  let yojson_of_t f v : Yojson.Safe.t = `Assoc (to_list v |> List.map (fun (key, vl) -> (key, f vl)))
  let t_of_yojson f (v : Yojson.Safe.t): 'a t = match v with
    | `Assoc ls -> List.map (fun (key,vl) -> (key, f vl)) ls |> of_list
    | _ -> raise (Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (Failure "expected an object", `Null))

  let pp fmt vl =
    pp
      ~pp_start:Format.(fun fmt () ->
        pp_open_hovbox fmt 1;
        pp_print_string fmt "{"
      )
      ~pp_stop:Format.(fun fmt () ->
        pp_print_string fmt "}";
        pp_close_box fmt ()
      )
      String.pp fmt vl
    
end


let (let+) = Lwt.bind
let () =
  Printexc.register_printer (function
      Ppx_yojson_conv_lib__Yojson_conv.Of_yojson_error(exn, yojson) ->
      Some (Printf.sprintf "exn %s: %s" (Printexc.to_string exn) (Format.to_string Yojson.Safe.pp yojson))
    | _ -> None
  )

type link = {
  rel: string;
  href: string option [@option]; 
  typ: string option [@key "type"] [@option];
  titles: string list[@default []];
  properties: yojson SMap.t [@default SMap.empty ];
} [@@yojson.allow_extra_fields] [@@deriving show, yojson]
(* see: https://datatracker.ietf.org/doc/html/rfc7033 *)


type t = {
  subject: string;
  links: link list [@default []];
  aliases: string list [@default []];
  properties: yojson SMap.t[@default SMap.empty];
} [@@yojson.allow_extra_fields] [@@deriving show, yojson]
               

type 'a activity = {
  context: string [@key "@context"] [@default "https://www.w3.org/ns/activitystreams"];
  id: string;
  actor: string;
  typ: string [@key "type"];
  obj: 'a SMap.t [@key "object"];
} [@@yojson.allow_extra_fields] [@@deriving show, yojson]


let activity = {
  context="https://www.w3.org/ns/activitystreams";
  id="https://localhost/create-hello-world";
  typ="Create";
  actor="https://localhost/actor";
  obj=SMap.of_list [
    "id", "https://my-example.com/hello-world";
    "type", "Note";
    "published", "2018-06-23T17:17:11Z";
    "attributedTo", "https://my-example.com/actor";
    "inReplyTo", "https://mastodon.social/@Gargron/100254678717223630";
    "content", "<p>Hello world</p>";
    "to", "https://www.w3.org/ns/activitystreams#Public"

  ]
} |> yojson_of_activity yojson_of_string

let private_key = X509.Private_key.generate `RSA
let pub_key = X509.Private_key.public private_key


let signed_string = "(request-target): post /inbox\nhost: localhost:4000\ndate: Sun, 06, Nov 1994 08:49:37 GMT"


let sign str = X509.PKCS12.create ~mac:`SHA256 str [] private_key |> X509.PKCS12.encode_der |> Cstruct.to_string
let () = print_endline @@ (Base64.encode (sign signed_string) |> Result.get_exn)


let main =
  let query = Uri.of_string "http://localhost:4000/.well-known/webfinger?resource=acct:example@localhost.com" in

  let+ (resp, body) = Cohttp_lwt_unix.Client.get query in
  print_endline @@ Format.to_string Cohttp.Response.pp_hum resp;

  let+ () =
  begin
    match body with
    | `Stream str ->
      let+ txt = Lwt_stream.fold (^) str "" in
      print_endline txt;
      let yojson = Yojson.Safe.from_string txt in
      print_endline @@ Yojson.Safe.to_string yojson;
      let _t = t_of_yojson yojson in
      print_endline @@ show _t;
      Lwt.return_unit
    | `Empty -> Lwt.return @@ print_endline "got nothing..."
    | `String res -> Lwt.return @@ print_endline @@ "got " ^ res
    | `Strings ls -> Lwt.return @@ List.iter print_endline ls
  end in


  Lwt.return ()


let () =
  Lwt_main.run @@ main
  
