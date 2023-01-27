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

let user_tag =
  Re.compile @@
  Re.(seq [
    group local_username;
    char '@';
    group (rep1 any)
  ])

let webfinger_format = lazy begin
  Re.compile @@
  Re.(seq [
    opt (str "acct:");
    group local_username;
    char '@';
    str (Lazy.force Params.domain |> Uri.host_with_default)
  ])
end

let local_user_id_format = lazy begin
  Re.compile @@
  Re.(seq [
    str (Lazy.force Url.user_base_url |> Uri.to_string);
    char '/';
    group local_username;
    opt (char '/')
  ])
end

let activity_format = lazy begin
  Re.compile @@
  Re.(seq [
    str (Lazy.force Url.activity_base_endpoint |> Uri.to_string);
    char '/';
    group uuid;
    opt (char '/')
  ])
end
