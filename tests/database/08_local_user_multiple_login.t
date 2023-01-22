  $ ./app_launcher.exe test.db local-user create-user "" "" false false joe shmoe
  { Operations.LocalUser.id = 1; username = "joe";
    password =
    "$argon2i$v=19$m=65536,t=3,p=2$SXnDqMKrUVvDoTLDkcSMwoDDt2DEnz/Clw$2W9AXigLNN5KI/ml7IWmdHJvdiRFwgb68anZlJ7pTww";
    display_name = None; about = None; manually_accepts_follows = false;
    is_admin = false; pubkey = <opaque>; privkey = <opaque> }
  $ ./app_launcher.exe test.db local-user create-user "" "" false false sally wally
  { Operations.LocalUser.id = 2; username = "sally";
    password =
    "$argon2i$v=19$m=65536,t=3,p=2$SXnDqMKrUVvDoTLDkcSMwoDDt2DEnz/Clw$E5GxSZPj+KR2QmfU2Iix7TbI30o63JHKZiS3NipAleI";
    display_name = None; about = None; manually_accepts_follows = false;
    is_admin = false; pubkey = <opaque>; privkey = <opaque> }
  $ ./app_launcher.exe test.db local-user create-user "hello" "" false true barry harry
  { Operations.LocalUser.id = 3; username = "barry";
    password =
    "$argon2i$v=19$m=65536,t=3,p=2$SXnDqMKrUVvDoTLDkcSMwoDDt2DEnz/Clw$JBJBtIOKa3vLb1FxDqKRZDGExleLl77rrk/QqMBT8mE";
    display_name = (Some "hello"); about = None;
    manually_accepts_follows = false; is_admin = true; pubkey = <opaque>;
    privkey = <opaque> }
  $ ./app_launcher.exe test.db local-user login-user joe shmoe
  { Operations.LocalUser.id = 1; username = "joe";
    password =
    "$argon2i$v=19$m=65536,t=3,p=2$SXnDqMKrUVvDoTLDkcSMwoDDt2DEnz/Clw$2W9AXigLNN5KI/ml7IWmdHJvdiRFwgb68anZlJ7pTww";
    display_name = None; about = None; manually_accepts_follows = false;
    is_admin = false; pubkey = <opaque>; privkey = <opaque> }
