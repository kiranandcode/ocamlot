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

(** * Collections  *)
let ordered_collection_page enc
      ({ id; prev; next; is_ordered; items; part_of; total_items }:
         _ Types.ordered_collection_page) =
  ap_obj "OrderedCollectionPage" [
    "id" @ id <: E.string;
    "next" @? next <: E.string;
    "prev" @? prev <: E.string;
    "partOf" @? part_of <: E.string;
    "totalItems" @? total_items <: E.int;
    (match is_ordered with
     | true -> "orderedItems"
     | false -> "items")  @ items <: E.list enc
  ]

let ordered_collection enc
      ({ id; total_items; contents }: _ Types.ordered_collection) =
  ap_obj "OrderedCollection" [
    "id" @? id <: E.string;
    "totalItems" @ total_items <: E.int;
    match contents with
    | `First page -> "first" @ page <: ordered_collection_page enc
    | `Items (true, items) -> "orderedItems" @ items <: E.list enc
    | `Items (false, items) -> "items" @ items <: E.list enc
  ]

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

let icon (url: string) =
  obj [
    "type" @ "Image" <: E.string;
    "url" @ url <: E.string;
  ]

let person ({ id; name; url; inbox; outbox;
              preferred_username; summary;
              manually_approves_followers;
              discoverable; followers; following;
              public_key=key; icon=i; raw=_ }: Types.person) =
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

    "icon" @? i <: icon;
  ]

let state = function
    `Pending -> E.string "pending"
  | `Cancelled -> E.string "cancelled"


let follow ({ id; actor; cc; object_; to_; state=st; raw=_ }: Types.follow) =
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

let attachment ({media_type; name; url; type_}: Types.attachment) =
  obj [
    "type" @? type_ <: E.string;
    "mediaType" @? media_type <: E.string;
    "name" @? name <: E.string;
    "url" @ url <: E.string;
  ]

let note ({ id; actor; to_; in_reply_to; cc; content; sensitive; source; summary;
            attachment=att;
            published; tags; raw=_ }: Types.note) =
  let att = match att with [] -> None | _ -> Some att in
  ap_obj "Note" [
    "id" @ id <: E.string;
    "actor" @ actor <: E.string;
    "attachment" @? att <: E.list attachment;
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

let block ({ id; obj; published; actor; raw=_ }: Types.block) =
  ap_obj "Block" [
    "id" @ id <: E.string;
    "object" @ obj <: E.string;
    "actor" @ actor <: E.string;
    "published" @? published <: ptime;
  ]

let like ({ id; actor; published; obj; raw=_ }: Types.like) =
  ap_obj "Like" [
    "id" @ id <: E.string;
    "actor" @ actor <: E.string;
    "object" @ obj <: E.string;
    "published" @? published <: ptime;
  ]


let core_obj : Types.core_obj E.encoder = function
  | `Follow f -> follow f
  | `Block b -> block b
  | `Note n -> note n
  | `Person p -> person p
  | `Like l -> like l
  | `Link r -> E.string r

let event enc : _ Types.event E.encoder = function
  | `Announce a -> announce enc a
  | `Undo u -> undo enc u
  | `Delete d -> delete enc d
  | `Create c -> create enc c
  | `Accept a -> accept enc a

let object_ : Types.obj E.encoder = function
  | #Types.core_obj as c -> core_obj c
  | #Types.core_event as e -> event core_obj e

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

module Nodeinfo = struct

  let software (s: Types.Nodeinfo.software) =
    obj [
      "name" @ s.name <: E.string;
      "version" @ s.version <: E.string;
    ]

  let usage_users ({ total; active_month; active_half_year }: Types.Nodeinfo.usage_users) =
    obj [
      "total" @ total <: E.int;
      "activeMonth" @ active_month <: E.int;
      "activeHalfyear" @ active_half_year <: E.int;
    ]

  let usage ({ local_posts; users } : Types.Nodeinfo.usage) =
    obj [
      "users" @ users <: usage_users;
      "localPosts" @ local_posts <: E.int
    ]

  let t ({ software=software'; protocols; inbound_services; outbound_services; usage=usage';
           open_registrations; metadata; raw=_ } : Types.Nodeinfo.t) =
    obj [
      "version" @ "2.0" <: E.string;
      "software" @ software' <: software;
      "protocols" @ protocols <: E.list E.string;
      "services" @ obj [
        "inbound" @ inbound_services <: E.list E.string;
        "outbound" @ outbound_services <: E.list E.string;
      ] <: Fun.id;
      "usage" @ usage' <: usage;
      "openRegistrations" @ open_registrations <: E.bool;
      "metadata" @? metadata <: Fun.id;
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
