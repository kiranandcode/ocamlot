(** * Events *)
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
  published: Ptime.t option;
  to_: string list;
  cc: string list;
  obj: 'a;
  raw: Yojson.Safe.t;
} [@@deriving show, eq]


type 'a accept = {
  id: string;
  actor: string;
  published: Ptime.t option;
  obj: 'a;
  raw: Yojson.Safe.t;
} [@@deriving show, eq]

type 'a undo = {
  id: string;
  actor: string;
  published: Ptime.t option;
  obj: 'a;
  raw: Yojson.Safe.t;
} [@@deriving show, eq]

type 'a delete = {
  id: string;
  actor: string;
  published: Ptime.t option;
  obj: 'a;
  raw: Yojson.Safe.t;
}
[@@deriving show, eq]

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
  raw: Yojson.Safe.t;
}  [@@deriving show, eq]

type follow = {
  id: string;
  actor: string;
  cc: string list;
  to_: string list;
  object_: string;
  state: [`Pending | `Cancelled ] option;
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

type block = {
  id: string;
  obj: string;
  actor: string;
  raw: Yojson.Safe.t;
} [@@deriving show, eq]

type like = {
  id: string;
  actor: string;
  obj: string;
  raw: Yojson.Safe.t;
}
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
