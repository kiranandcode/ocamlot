let local_username =
  Re.(rep1 (alt [rg 'a' 'z'; rg '0' '9'; rg 'A' 'Z'; char '_'; char '.'; char '-']))


let user_tag _config =
  Re.(seq [
    group local_username;
    char '@';
    group (rep1 any)
  ]) 
  

let webfinger_format config =
  Re.(seq [
    opt (str "acct:");
    group local_username;
    char '@';
    str (Params.domain config |> Uri.host_with_default)
  ])

let local_user_id_format config =
  Re.(seq [
    str (Url.user_base_url config |> Uri.to_string);
    char '/';
    group local_username;
    opt (char '/')
  ])
