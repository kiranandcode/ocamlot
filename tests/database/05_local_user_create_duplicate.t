  $ ./app_launcher.exe test.db local-user create-user "" "" false false joe shmoe
  { Operations.LocalUser.id = 1; username = "joe";
    password =
    "$argon2i$v=19$m=65536,t=3,p=2$SXnDqMKrUVvDoTLDkcSMwoDDt2DEnz/Clw$2W9AXigLNN5KI/ml7IWmdHJvdiRFwgb68anZlJ7pTww";
    display_name = None; about = None; manually_accepts_follows = false;
    is_admin = false; pubkey = <opaque>; privkey = <opaque> }
  $ ./app_launcher.exe test.db local-user create-user "" "" false false joe random
  Fatal error: exception Response from <sqlite3:///test.db> failed: UNIQUE constraint failed: local_user.username. Query: "INSERT INTO local_user (username, password, manually_accept_follows, is_admin, pubkey, privkey) VALUES (?1, ?2, ?3, ?4, ?5, ?6)".
  [2]
