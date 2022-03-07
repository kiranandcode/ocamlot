[@@@warning "-33"]
open Containers

let follow = IO.with_in "../resources/examples/pleroma-follow.json"
             IO.read_all
         |> Yojson.Safe.from_string

module D = Decoders_yojson.Safe.Decode

let timestamp =
  let open D in
  let* time = string in
  match Ptime.of_rfc3339 time |> Ptime.rfc3339_error_to_msg with
  | Ok (t, _, _) -> succeed t
  | Error `Msg err -> fail err

let constant ?msg target =
  let open D in
  let* str = string in
  if String.equal str target
  then succeed ()
  else match msg with
    | None -> fail (Printf.sprintf "expected %s received %s" target str)
    | Some msg -> fail (Printf.sprintf msg str)

let field_or_default field' decoder default =
  let open D in
  let+ field = field_opt field' decoder in
  Option.value ~default field

type 'a create = {
  id: string;
  actor: string;
  published: Ptime.t;
  to_: string list;
  cc: string list;
  direct_message: bool;
  obj: 'a;
  raw: Yojson.Safe.t;
} [@@deriving show, eq]

type note = {
  id: string;
  actor: string;
  to_: string list;
  cc: string list;
  content: string;
  source: string;
  summary: string;
  published: Ptime.t;
  raw: Yojson.Safe.t;
} [@@deriving show, eq]


let note = 
  let open D in
  let* () = constant ~msg:"expected Note (received %s)" "Note"
  and* id = field "id" string
  and* actor = field "actor" string
  and* to_ = field "to" (list string)
  and* cc = field "cc" (list string)
  and* content = field "content" string
  and* source = field "source" string
  and* summary = field "summary" string
  and* published = field "published" timestamp
  and* raw = value in
  succeed { id; actor; to_; cc; content; source; summary; published; raw }


let create obj =
  let open D in
  let* () = field "type" @@ constant ~msg:"expected create object (received %s)" "Create"
  and* id = field "id" string
  and* actor = field "actor" string 
  and* direct_message = field_or_default "direct" bool false
  and* published = field "published" timestamp
  and* to_ = field "to" (list string)
  and* cc = field_or_default "cc" (list string) []
  and* obj = field "object" obj
  and* raw = value in

  succeed ({
    id; actor; published;
    to_; cc;
    direct_message;
    obj;
    raw;
  }: _ create)

       



let () = Yojson.Safe.to_string ~std:true follow
         |> print_endline
