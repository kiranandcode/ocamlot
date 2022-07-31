let () = declare_schema "../../resources/schema.sql"
open Utils

(* see ./resources/schema.sql:Activity *)
type%sql.generate activity = SQL [@schema "Activity"]

(* see ./resources/schema.sql:Follows *)
type%sql.generate follow = SQL [@schema "Follows"]

(* see ./resources/schema.sql:Like *)
type%sql.generate like = SQL [@schema "Likes"]

(* see ./resources/schema.sql:LocalUser *)
type%sql.generate local_user = SQL [@schema "LocalUser"]

(* see ./resources/schema.sql:Post *)
type%sql.generate post = SQL [@schema "Posts"]

(* see ./resources/schema.sql:RemoteInstance *)
type%sql.check[@schema "RemoteInstance"] remote_instance = {
  id: int64;                              (* unique internal id of user *)
  url: string;                            (* url to instance *)
  mutable last_unreachable: timestamp option;    (* time since the sever was unreachable *)
}

(* see ./resources/schema.sql:RemoteUser *)
type%sql.generate remote_user = SQL [@schema "RemoteUser"]

(* see ./resources/schema.sql:Tag *)
type%sql.generate tag = SQL [@schema "Tags"]

