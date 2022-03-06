open Containers
module JS = Yojson.Safe

let uri_of_raw path = path
let assoc vls : JS.t = `Assoc vls
let string str : JS.t = `String str
let list vls : JS.t = `List vls
let uri vl : JS.t = `String (Uri.to_string (uri_of_raw vl))

let with_path path uri =
  let old_path = Uri.path uri in
  let old_path =
    if not @@ String.suffix ~suf:"/" old_path
    then old_path ^ "/"
    else old_path in
  Uri.with_path uri (old_path ^ path)

let with_fragment fragment uri = Uri.with_fragment uri (Some fragment)

module ContentType = struct
  let ld_json_activity_streams = "application/ld+json; profile=\"https://www.w3.org/ns/activitystreams\""

  let ld_json = "application/ld+json"
  let activity_json = "application/activity+json"
  let plain_json = "application/json"
  let html = "text/html"
  let any = "*/*"

  let content_types = [
    ld_json_activity_streams, `JSON;
    ld_json, `JSON;
    activity_json, `JSON;
    plain_json, `JSON;
    html, `HTML;
    any, `HTML;
  ]

  let of_string content_type =
    List.find_opt
      (fun (str, _) ->
         print_endline @@ "looking up " ^ str ^ " matches " ^ content_type;
         String.prefix ~pre:str content_type)
      content_types
    |> Option.map snd

end

module ActivityStreams = struct



  let context =
    "@context", list [
      string "https://www.w3.org/ns/activitystreams";
      string "https://w3id.org/security/v1"
    ]

end


module LocalUser = struct

  module PublicKey = struct

    let of_local_user config actor =
      let username = Database.LocalUser.username actor in
      assoc [
        "id", uri (Configuration.Url.user config username |> with_fragment "main-key");
        "owner", uri (Configuration.Url.user config username);
        "publicKeyPem", string (Database.LocalUser.pubkey actor)
      ]      

  end


  let of_local_user config (actor: Database.LocalUser.t) =
    let username = Database.LocalUser.username actor in
    assoc [
      ActivityStreams.context;
      "id", uri (Configuration.Url.user config username);
      "type", string "Person";
      "name", string (Database.LocalUser.display_name actor);
      "preferredUsername", string (Database.LocalUser.display_name actor);
      "inbox", uri (Configuration.Url.user_inbox config username);
      "publicKey", PublicKey.of_local_user config actor
    ]

end

module Webfinger = struct

  let profile_page url = assoc [
    "href", uri url;
    "rel", string "http://webfinger.net/rel/profile-page";
    "type", string "text/html";
  ]

  let self_link ty url = assoc [
    "href", uri url;
    "rel", string "self";
    "type", string ty
  ]    

  let activity_json_self config username =
    self_link ContentType.activity_json (Configuration.Url.user config username)
  let activitystreams_self config username =
    self_link ContentType.ld_json_activity_streams (Configuration.Url.user config username)

  let of_local_user config (actor: Database.LocalUser.t) =
    let username = Database.LocalUser.username actor in
    assoc [
      "subject", string (Configuration.Format.user_specifier config username);
      "aliases", list [ uri (Configuration.Url.user config username) ];
      "links", list [
        profile_page (Configuration.Url.user_profile_page config username);
        activity_json_self config username;
        activitystreams_self config username;
      ]
    ]

end
