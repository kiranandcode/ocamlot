type t = {
  domain: string;

  database_path: string;

  about_this_instance: Omd.doc;

  port: int;

  debug: bool
}

let default_about_this_instance = {|
# About this instance

The site administrator for this OCamlot instance has not configured the about this page.

[OCamlot](https://codeberg/gopiandcode/ocamlot) is Free/Libre (as in FREEDOM) software that supports the FREEDOM of its users!

Donate to the FSF if you wish to support FREEDOM: [Free Software Foundation](www.fsf.org)
|}

let create ?(about_this_instance=default_about_this_instance) ?(debug=false) ?(port=7331) ~database_path  domain = {
  domain;
  database_path;
  about_this_instance=Omd.of_string about_this_instance;
  port;
  debug
}

let about_this_instance v = v.about_this_instance
let domain v = Uri.of_string ("http://" ^ v.domain)
let database_path v = v.database_path
let port v = v.port
let debug v = v.debug
