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

  let convert_to (actor: Operations.LocalUser.t) : Activitypub.Types.person =
    let username = actor.Operations.LocalUser.username in
    let convert_pubkey actor : Activitypub.Types.public_key =
      let username = actor.Operations.LocalUser.username in {
        id=uri (Configuration.Url.user username |> with_fragment "main-key");
        owner=uri (Configuration.Url.user username);
        pem=Cstruct.to_string (X509.Public_key.encode_pem actor.Operations.LocalUser.pubkey);
      } in {
      id= uri (Configuration.Url.user username);
      name= Some Operations.LocalUser.(actor.username);
      url = Some (uri (Configuration.Url.user_profile_page username));
      preferred_username=(actor.Operations.LocalUser.display_name);
      inbox = uri (Configuration.Url.user_inbox username);
      outbox = uri (Configuration.Url.user_outbox username);
      summary = (actor.Operations.LocalUser.about);
      public_key = convert_pubkey actor;
      manually_approves_followers =
        actor.Operations.LocalUser.manually_accepts_follows;
      discoverable = true;
      followers = Some (uri (Configuration.Url.user_followers username));
      following = Some (uri (Configuration.Url.user_following username));
      icon = Option.map Fun.(uri % Configuration.Url.image_url) actor.profile_picture;
      raw = `Null
    }

end

module Webfinger = struct

  
  let self_link ty url = Activitypub.Types.Webfinger.Self (ty, uri url)

  let activity_json_self username =
    self_link `ActivityJson (Configuration.Url.user username)
  let activitystreams_self username =
    self_link `ActivityJsonLd (Configuration.Url.user username)

  let construct_query_result_for (actor: Operations.LocalUser.t) : Activitypub.Types.Webfinger.query_result =
    let username = actor.Operations.LocalUser.username in
    {
      subject=Configuration.Format.user_specifier username;
      aliases=[uri (Configuration.Url.user username)];
      links=Activitypub.Types.Webfinger.[
        ProfilePage (`Html, uri @@ Configuration.Url.user_profile_page username);
        activity_json_self username;
        activitystreams_self username;
      ]
    }

end
