type t = {
  domain: string;

  database_path: string;
}

let create ~domain ~database_path = {
  domain;
  database_path;
}

let domain v = Uri.make ~scheme:"https" ~host:v.domain ()
let database_path v = v.database_path
