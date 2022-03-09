[@@@warning "-33"]
open Containers

let follow_cancel = IO.with_in "../resources/examples/pleroma-follow-cancel.json"
             IO.read_all
         |> Yojson.Safe.from_string

module D = Decoders_yojson.Safe.Decode

let timestamp =
  let open D in
  let* time = string in
  match Ptime.of_rfc3339 time |> Ptime.rfc3339_error_to_msg with
  | Ok (t, _, _) -> succeed t
  | Error `Msg err -> fail err

let singleton_or_list dec =
  D.(one_of ["singleton", (dec >|= fun v -> [v]);
             "list", list dec;
             "null", null >|= fun () -> []])  

let lossy_list_of dec =
  let open D in
  list (one_of ["known", (dec >|= fun v -> `Value v); "unknown", value >|= fun v -> `Raw v])

let id = D.(one_of ["string", string; "id", field "id" string])

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
  published: Ptime.t option;
  to_: string list;
  cc: string list;
  direct_message: bool;
  obj: 'a;
  raw: Yojson.Safe.t;
} [@@deriving show, eq]


type 'a announce = {
  id: string;
  actor: string;
  to_: string list;
  cc: string list;
  obj: 'a;
  raw: Yojson.Safe.t;
} [@@deriving show, eq]


type tag = {
  ty: [`Mention | `Hashtag ];
  href: string;
  name: string;
} [@@deriving show, eq]

type note = {
  id: string;
  actor: string;
  to_: string list;
  in_reply_to: string option;
  cc: string list;
  content: string;
  sensitive: bool;
  source: string option;
  summary: string option;
  published: Ptime.t option;
  tags: [ `Raw of Yojson.Safe.t | `Value of tag ] list;
  raw: Yojson.Safe.t;
} [@@deriving show, eq]

type follow = {
  id: string;
  actor: string;
  cc: string list;
  object_: string;
  state: [`Pending ] option;
} [@@deriving show, eq]

type person = {
  id: string;
  name: string option;
  url: string option;

  preferred_username: string option;

  inbox: string;
  outbox: string;

  summary: string option;

  public_key: string;

  manually_approves_followers: bool;
  
  discoverable: bool;
  followers: string option;
  following: string option;
  icon: string option;
  raw: Yojson.Safe.t;
}  [@@deriving show, eq]

type 'a accept = {
  id: string;
  actor: string;
  obj: 'a;
  raw: Yojson.Safe.t;
} [@@deriving show, eq]

type block = {
  id: string;
  obj: string;
  actor: string;
  raw: Yojson.Safe.t;
} [@@deriving show, eq]

type 'a delete = {
  id: string;
  actor: string;
  obj: 'a;
  raw: Yojson.Safe.t;
}
[@@deriving show, eq]

type like = {
  id: string;
  actor: string;
  obj: string;
  raw: Yojson.Safe.t;
}
[@@deriving show, eq]

type 'a undo = {
  id: string;
  actor: string;
  obj: 'a;
  raw: Yojson.Safe.t;
} [@@deriving show, eq]



let mention =
  let open D in
  let* () = field "type" @@ constant ~msg:"expected Mention (received %s)" "Mention"
  and* href = field "href" string
  and* name = field "name" string in
  succeed {ty=`Mention; href;name}
let hashtag =
  let open D in
  let* () = field "type" @@ constant ~msg:"expected Hashtag (received %s)" "Hashtag"
  and* href = field "href" string
  and* name = field "name" string in
  succeed {ty=`Hashtag; href;name}

let tag =
  let open D in
  let* ty = field "type" string in
  match ty with
  | "Mention" -> mention
  | "Hashtag" -> hashtag
  | _ -> fail (Printf.sprintf "unknown tag %s" ty)

let undo obj =
  let open D in
  let* () = field "type" @@ constant ~msg:"expected Undo (received %s)" "Undo"
  and* id = field "id" string
  and* actor = field "actor" id
  and* obj = field "object" obj
  and* raw = value in
  succeed ({id;actor;obj;raw}: _ undo)

let like =
  let open D in
  let* () = field "type" @@ constant ~msg:"expected Like (received %s)" "Like"
  and* id = field "id" string
  and* actor = field "actor" id
  and* obj = field "object" id
  and* raw = value in
  succeed {id; actor; obj; raw}


let tombstone =
  let open D in
  let* () = field "type" @@ constant ~msg:"expected Tombstone (received %s)" "Tombstone"
  and* id = field "id" string in
  succeed id

let delete obj =
  let open D in
  let* () = field "type" @@ constant ~msg:"expected Delete (received %s)" "Delete"
  and* id = field "id" string
  and* actor = field "actor" id
  and* obj = field "object" obj
  and* raw = value in
  succeed {id;actor;obj;raw}

let block =
  let open D in
  let* () = field "type" @@ constant ~msg:"expected Block (received %s)" "Block"
  and* id = field "id" string
  and* obj = field "object" string
  and* actor = field "actor" id
  and* raw = value in
  succeed {id; obj;actor;raw}


let accept obj =
  let open D in
  let* () = field "type" @@ constant ~msg:"expected Accept (received %s)" "Accept"
  and* id = field "id" string
  and* actor = field "actor" id
  and* obj = field "object" obj
  and* raw = value in
  succeed {id;actor;obj;raw}

let person =
  let open D in
  let* () = field "type" @@ constant ~msg:"expected Person (received %s)" "Person"
  and* id = field "id" string
  and* name = field_opt "name" string
  and* url = field_or_default "url" (nullable string) None
  and* preferred_username = field_opt "preferredUsername" string
  and* inbox = field "inbox" string
  and* outbox = field "outbox" string
  and* summary = field_opt "summary" string
  and* public_key = at ["publicKey"; "publicKeyPem"] string 
  and* manually_approves_followers =
    field_or_default "manuallyApprovesFollowers" bool false
  and* discoverable = field_or_default "discoverable" bool false
  and* followers = field_opt "followers" string
  and* following = field_opt "following" string
  and* icon = maybe (at ["icon";"url"] string)
  and* raw = value in
  succeed {
    id;
    name;
    url;

    preferred_username;

    inbox;
    outbox;

    summary;

    public_key;

    manually_approves_followers;

    discoverable;
    followers;
    following;
    icon;
    raw;
  }

let note = 
  let open D in
  let* () = field "type" @@ constant ~msg:"expected Note (received %s)" "Note"
  and* id = field "id" string
  and* actor = one_of ["actor", field "actor" id; "attributed_to", field "attributedTo" id]
  and* to_ = field "to" (singleton_or_list string)
  and* in_reply_to = field_or_default "inReplyTo" (nullable string) None
  and* cc = field_or_default "cc" (singleton_or_list string) []
  and* content = field "content" string
  and* source = field_opt "source"
                  (one_of ["string", string; "multi-encode", field "content" string])
  and* summary = field_or_default "summary" (nullable string) None
  and* sensitive = field_or_default "sensitive" bool false
  and* published = field_opt "published" timestamp
  and* tags = field_or_default "tag" (lossy_list_of tag) []
  and* raw = value in
  succeed { id; actor; in_reply_to; to_; cc; sensitive; content; source; summary; tags; published; raw }

let follow =
  let open D in
  let* () = field "type" @@ constant ~msg:"expected create object (received %s)" "Follow"
  and* actor = field "actor" id
  and* cc = field_or_default "cc" (singleton_or_list string) []
  and* id = field "id" string
  and* object_ = field "object" string
  and* state = field_opt "state" (string >>= function "pending" -> succeed `Pending
                                                 | _ -> fail "unknown status") in
  succeed {actor; cc; id; object_; state}

let announce obj =
  let open D in
  let* () = field "type" @@ constant ~msg:"expected create object (received %s)" "Announce"
  and* actor = field "actor" id
  and* id = field "id" string
  and* to_ = field "to" (singleton_or_list string)
  and* cc = field_or_default "cc" (singleton_or_list string) []
  and* obj = field "object" obj
  and* raw = value in
  succeed {id; actor; to_; cc; obj; raw}

let create obj =
  let open D in
  let* () = field "type" @@ constant ~msg:"expected create object (received %s)" "Create"
  and* id = field "id" string
  and* actor = field "actor" id
  and* direct_message = field_or_default "direct" bool false
  and* published = field_opt "published" timestamp
  and* to_ = field_or_default "to" (singleton_or_list string) []
  and* cc = field_or_default "cc" (singleton_or_list string) []
  and* obj = field "object" obj
  and* raw = value in

  succeed ({
    id; actor; published;
    to_; cc;
    direct_message;
    obj;
    raw;
  }: _ create)

       



let () = Yojson.Safe.to_string ~std:true follow_cancel
         |> print_endline
