open Containers

let uri u = Uri.to_string u
let with_path path uri =
  let old_path = Uri.path uri in
  let old_path =
    if not @@ String.suffix ~suf:"/" old_path
    then old_path ^ "/"
    else old_path in
  Uri.with_path uri (old_path ^ path)

let with_fragment fragment uri = Uri.with_fragment uri (Some fragment)

module LocalUser = struct

  let convert_to config (actor: Local_user.t) : Activitypub.Types.person =
    let username = Local_user.username actor in
    let convert_pubkey config actor : Activitypub.Types.public_key =
      let username = Local_user.username actor in {
        id=uri (Configuration.Url.user config username |> with_fragment "main-key");
        owner=uri (Configuration.Url.user config username);
        pem=Local_user.pubkey actor;
      } in {
      id= uri (Configuration.Url.user config username);
      name= Some actor.username;
      url = Some (uri (Configuration.Url.user_profile_page config username));
      preferred_username= Some (Local_user.display_name actor);
      inbox = uri (Configuration.Url.user_inbox config username);
      outbox = uri (Configuration.Url.user_outbox config username);
      summary = Some "SUMMARIES NOT CURRENTLY SUPPORTED BY OCAMLOT";
      public_key = convert_pubkey config actor;
      manually_approves_followers = false;
      discoverable = true;
      followers = None;
      following = None;
      icon = None;
      raw = `Null
    }

end

module Webfinger = struct

  
  let self_link ty url = Activitypub.Types.Webfinger.Self (ty, uri url)

  let activity_json_self config username =
    self_link `ActivityJson (Configuration.Url.user config username)
  let activitystreams_self config username =
    self_link `ActivityJsonLd (Configuration.Url.user config username)

  let construct_query_result_for config (actor: Local_user.t) : Activitypub.Types.Webfinger.query_result =
    let username = Local_user.username actor in
    {
      subject=Configuration.Format.user_specifier config username;
      aliases=[uri (Configuration.Url.user config username)];
      links=Activitypub.Types.Webfinger.[
        ProfilePage (`Html, uri @@ Configuration.Url.user_profile_page config username);
        activity_json_self config username;
        activitystreams_self config username;
      ]
    }

end
