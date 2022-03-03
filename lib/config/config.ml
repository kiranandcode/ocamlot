type t = {
  domain: string;
}

let create ~domain = {
  domain
}

let domain v = Uri.make ~scheme:"https" ~host:v.domain ()
