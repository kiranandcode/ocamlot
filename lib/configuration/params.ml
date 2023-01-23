type 'task t = {
  domain: string;
  (** domain on which the server is running - used to sign messages, so should not be localhost. *)

  user_image_path: string;
  (** path at which the user images are stored (created if it does not exist). *)

  database_path: string;
  (** path at which the server database is located (created if it does not exist).  *)

  about_this_instance: Omd.doc;
  (** Markdown text show to users when logging in or registering an account. *)

  port: int;
  (** port on which the server should run. *)

  certificate_file: string option;
  (** certificate file to use for tls encryption. Both [certificate_file] and [key_file] must be provided for tls to be enabled. *)

  key_file: string option;
  (** key file to use for tls encryption. Both [certificate_file] and [key_file] must be provided for tls to be enabled. *)

  debug: bool;
  (** whether to run in debug mode.  *)


  mutable send_task: 'task option -> unit;
}

let default_about_this_instance = {|
# About this instance

The site administrator for this OCamlot instance has not configured the about this page.

[OCamlot](https://codeberg/gopiandcode/ocamlot) is Free/Libre (as in FREEDOM) software that supports the FREEDOM of its users!

Donate to the FSF if you wish to support FREEDOM: [Free Software Foundation](www.fsf.org)
|}

let create ?key_file ?certificate_file ?(about_this_instance=default_about_this_instance) ?(user_image_path="./user-images/")
      ?(debug=false) ?(port=7331) ~database_path  domain = {
  domain;
  user_image_path;
  database_path;
  about_this_instance=Omd.of_string about_this_instance;
  port;
  certificate_file; key_file;
  debug;
  send_task=fun _ -> ()
}

let send_task config task = config.send_task (Some task)
let set_task_fn config task_fn = config.send_task <- task_fn

let is_tls_enabled v = Option.is_some v.certificate_file && Option.is_some v.key_file
let certificate_file v =
  if not (is_tls_enabled v)
  then None
  else v.certificate_file
let key_file v =
  if not (is_tls_enabled v)
  then None
  else v.key_file
  
let about_this_instance v = v.about_this_instance
let host v = v.domain
let domain v = Uri.of_string ("https://" ^ v.domain)
let database_path v = v.database_path
let port v = v.port
let debug v = v.debug
let user_image_path v = v.user_image_path
