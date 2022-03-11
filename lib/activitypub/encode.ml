module E = Decoders_yojson.Safe.Encode


let (<:) = function
  | (_, None) -> fun _ -> []
  | (field, Some vl) -> fun ty -> [field, ty vl]
let (@) field vl = (field, Some vl)
let (@?) field vl = (field, vl)

let ptime time = E.string (Ptime.to_rfc3339 ~tz_offset_s:0 time)
let obj ls = E.obj @@ List.flatten ls
let ap_obj ty ls =
  E.obj (Constants.ActivityStreams.context :: ("type", E.string ty) :: List.flatten ls)

let or_raw conv = function
  | `Raw v -> v
  | `Value v -> conv v


(** * Events *)

let create enc ({ id; actor; published; to_; cc; direct_message; obj; raw=_ }:
                  _ Types.create) =
  ap_obj "Create" [
    "id" @ id <: E.string;
    "actor" @ actor <: E.string;
    "published" @? published <: ptime;
    "to" @ to_ <: E.(list string);
    "cc" @ cc <: E.(list string);
    "directMessage"  @ direct_message <: E.bool;
    "object" @ obj <: enc;
  ]

let announce enc ({ id; actor; published; to_; cc; obj; raw=_ } : _ Types.announce) =
  ap_obj "Announce" [
    "id" @ id <: E.string;
    "actor" @ actor <: E.string;
    "published" @? published <: ptime;
    "to" @ to_ <: E.(list string);
    "cc" @ cc <: E.(list string);
    "object" @ obj <: enc;
  ]

let accept enc ({ id; actor; published; obj; raw=_ } : _ Types.accept) =
  ap_obj "Accept" [
    "id" @ id <: E.string;
    "actor" @ actor <: E.string;
    "published" @? published <: ptime;
    "object" @ obj <: enc;
  ]

let undo enc ({ id; actor; published; obj; raw=_ } : _ Types.undo) =
  ap_obj "Undo" [
    "id" @ id <: E.string;
    "actor" @ actor <: E.string;
    "published" @? published <: ptime;
    "object" @ obj <: enc;
  ]

let delete enc ({ id; actor; published; obj; raw=_ } : _ Types.delete) =
  ap_obj "Delete" [
    "id" @ id <: E.string;
    "actor" @ actor <: E.string;
    "published" @? published <: ptime;
    "object" @ obj <: enc;
  ]

(** * Objects *)

let public_key (key: Types.public_key) =
  obj [
    "id" @ key.id <: E.string;
    "owner" @ key.owner <: E.string;
    "publicKeyPem" @ key.pem <: E.string;
  ]

let person ({ id; name; url; inbox; outbox;
              preferred_username; summary;
              manually_approves_followers;
              discoverable; followers; following;
              public_key=key; icon=_; raw=_ }: Types.person) =
  ap_obj "Person" [

    "id" @ id <: E.string;

    "name" @? name <: E.string;
    "url" @? url <: E.string;

    "preferredUsername" @? preferred_username <: E.string;

    "inbox" @ inbox <: E.string;
    "outbox" @ outbox <: E.string;

    "summary" @? summary <: E.string;

    "publicKey" @ key <: public_key;

    "manuallyApprovesFollowers" @ manually_approves_followers <: E.bool;

    "discoverable" @ discoverable <: E.bool;

    "followers" @? followers <: E.string;
    "following" @? following <: E.string;

  ]

let state = function
    `Pending -> E.string "pending"
  | `Cancelled -> E.string "cancelled"


let follow ({ id; actor; cc; object_; to_; state=st }: Types.follow) =
  ap_obj "Follow" [
    "id" @ id <: E.string;
    "actor" @ actor <: E.string;
    "to" @ to_ <: E.list E.string;
    "cc" @ cc <: E.list E.string;
    "object" @ object_ <: E.string;
    "state" @? st <: state;
  ]

let tag ({ ty; href; name }: Types.tag) =
  ap_obj (match ty with `Mention -> "Mention" | `Hashtag -> "Hashtag") [
    "href" @ href <: E.string;
    "name" @ name <: E.string;
  ]

let note ({ id; actor; to_; in_reply_to; cc; content; sensitive; source; summary;
            published; tags; raw=_ }: Types.note) =
  ap_obj "Note" [
    "id" @ id <: E.string;
    "actor" @ actor <: E.string;
    "to" @ to_ <: E.list E.string;
    "inReplyTo" @? in_reply_to <: E.string;
    "cc" @ cc <: E.list E.string;
    "content" @ content <: E.string;
    "sensitive" @ sensitive <: E.bool;
    "source" @? source <: E.string;
    "summary" @? summary <: E.string;
    "published" @? published <: ptime;
    "tags" @ tags <: E.list (or_raw tag);
  ]

let block ({ id; obj; actor; raw=_ }: Types.block) =
  ap_obj "Block" [
    "id" @ id <: E.string;
    "object" @ obj <: E.string;
    "actor" @ actor <: E.string;
  ]

let like ({ id; actor; obj; raw=_ }: Types.like) =
  ap_obj "Like" [
    "id" @ id <: E.string;
    "actor" @ actor <: E.string;
    "object" @ obj <: E.string;
  ]

module Webfinger = struct

  let ty = function
    | `ActivityJson -> E.string Constants.ContentType.activity_json
    | `Html -> E.string Constants.ContentType.html
    | `ActivityJsonLd -> E.string Constants.ContentType.ld_json_activity_streams
    | `Json -> E.string Constants.ContentType.plain_json

  let link = function
    | Types.Webfinger.Self (t, href) -> obj [
      "href" @ href <: E.string;
      "rel" @ Constants.Webfinger.self_rel <: E.string;
      "type" @ t <: ty;
    ]
    | ProfilePage (t, href) ->
      obj [
        "href" @ href <: E.string;
        "rel" @ Constants.Webfinger.profile_page <: E.string;
        "type" @ t <: ty;
      ]
    | OStatusSubscribe template -> obj [
      "rel" @ Constants.Webfinger.profile_page <: E.string;
      "template" @ template <: E.string;
    ]

  let query_result ({subject;aliases;links}: Types.Webfinger.query_result) =
    obj [
      "subject" @ subject <: E.string;
      "aliases" @ aliases <: E.(list string);
      "links" @ links <: E.list link;
    ]

end

(* module Build (S: sig
 *     type user
 * 
 *     val owner: user -> Uri.t
 * 
 *   end) = struct
 * 
 * end *)
