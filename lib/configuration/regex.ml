let local_username =
  Re.(rep1 (alt [rg 'a' 'z'; rg '0' '9'; rg 'A' 'Z'; char '_'; char '.'; char '-']))

let uuid =
  Re.(rep1 (alt [rg 'a' 'z'; rg '0' '9'; rg 'A' 'Z'; char '-']))

let domain =
  let open Re in
  let alnum = alt [
    rg 'a' 'z';
    rg 'A' 'Z';
    rg '0' '9';
  ] in
  let alnum_ = alt [alnum; char '-'] in
  let segment = alt [ alnum; seq [ alnum; rep1 alnum_; alnum ] ] in
  let segment_dot = seq [segment; char '.' ] in
  seq [rep segment_dot; segment]

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

let activity_format config =
  Re.(seq [
    str (Url.activity_base_endpoint config |> Uri.to_string);
    char '/';
    group uuid;
    opt (char '/')
  ])
