type yojson = Yojson.Safe.t
let pp_yojson fmt vl = Yojson.Safe.pretty_print fmt vl
let equal_yojson l r = Yojson.Safe.equal l r

(** * Events *)
type 'a create = {
  id: string;
  actor: string;
  published: Ptime.t option;
  to_: string list;
  cc: string list;
  direct_message: bool;
  obj: 'a;
  raw: yojson;
} [@@deriving show, eq]


type 'a announce = {
  id: string;
  actor: string;
  published: Ptime.t option;
  to_: string list;
  cc: string list;
  obj: 'a;
  raw: yojson;
} [@@deriving show, eq]


type 'a accept = {
  id: string;
  actor: string;
  published: Ptime.t option;
  obj: 'a;
  raw: yojson;
} [@@deriving show, eq]

type 'a undo = {
  id: string;
  actor: string;
  published: Ptime.t option;
  obj: 'a;
  raw: yojson;
} [@@deriving show, eq]

type 'a delete = {
  id: string;
  actor: string;
  published: Ptime.t option;
  obj: 'a;
  raw: yojson;
}
[@@deriving show, eq]

type 'a event = [
    `Create of 'a create
  | `Announce of 'a announce
  | `Accept of 'a accept
  | `Undo of 'a undo
  | `Delete of 'a delete
] [@@deriving show, eq]


(** * Objects *)
type public_key = {
  id: string;
  owner: string;
  pem: string;
} [@@deriving show, eq]

type person = {
  id: string;
  name: string option;
  url: string option;

  preferred_username: string option;

  inbox: string;
  outbox: string;

  summary: string option;

  public_key: public_key;

  manually_approves_followers: bool;

  discoverable: bool;
  followers: string option;
  following: string option;
  icon: string option;
  raw: yojson;
}  [@@deriving show, eq]

type follow = {
  id: string;
  actor: string;
  cc: string list;
  to_: string list;
  object_: string;
  state: [`Pending | `Cancelled ] option;
  raw: yojson;
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
  tags: [ `Raw of yojson | `Value of tag ] list;
  raw: yojson;
} [@@deriving show, eq]

type block = {
  id: string;
  obj: string;
  published: Ptime.t option;
  actor: string;
  raw: yojson;
} [@@deriving show, eq]

type like = {
  id: string;
  actor: string;
  published: Ptime.t option;
  obj: string;
  raw: yojson;
}
[@@deriving show, eq]

type core_obj = [
    `Person of person
  | `Follow of follow
  | `Note of note
  | `Block of block
  | `Like of like
] [@@deriving show, eq]

type core_event = core_obj event
[@@deriving show, eq]

type obj = [ core_obj | core_event ]
[@@deriving show, eq]

module Webfinger = struct

  type ty = [ `Html | `Json | `ActivityJson | `ActivityJsonLd ]
  [@@deriving show, eq]

  type link =
    | Self of ty * string
    | ProfilePage of ty * string
    | OStatusSubscribe of string
  [@@deriving show, eq]

  type query_result = {
    subject: string;
    aliases: string list;
    links: link list;
  }
  [@@deriving show, eq]

end
