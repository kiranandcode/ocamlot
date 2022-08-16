type t = {
  domain: string;

  database_path: string;

  port: int;

  debug: bool
}

let create ?(debug=false) ?(port=7331) ~database_path domain = {
  domain;
  database_path;
  port;
  debug
}

let domain v = Uri.of_string ("http://" ^ v.domain)
let database_path v = v.database_path
let port v = v.port
let debug v = v.debug
