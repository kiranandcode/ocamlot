
type t = 
  | CreateRemoteNote of {
    author : string;
    direct_message: bool;
    note: Activitypub.Types.note;
  }

  | HandleUndoFollow of {
      follow_id: string;
    }

  | HandleUndo of {
      author : string;
      obj: string;
    }

  | HandleRemoteFollow of {
      id: string;
      actor: string;
      target: Database.LocalUser.t;
      raw: Yojson.Safe.t;
    }

  | HandleAcceptFollow of { follow_id: string; }

  | FollowRemoteUser of {
      user: Database.LocalUser.t;
      username: string;
      domain: string;
    }

  | UnfollowRemoteUser of {
      user: Database.LocalUser.t;
      username: string;
      domain: string;
    }

  | HandleRemoteLikeOfLocalPost of {
      id: string;
      published: Ptime.t;
      target: Database.Posts.t;
      author: string;
      raw_data: Yojson.Safe.t;
    }
  | HandleRemoteLikeOfRemotePost of {
      id: string;
      published: Ptime.t;
      target: string;
      author: string;
      raw_data: Yojson.Safe.t;
    }
  | HandleRemoteReboostOfLocalPost of {
      id: string;
      published: Ptime.t;
      target: Database.Posts.t;
      author: string;
      raw_data: Yojson.Safe.t;
    }
  | HandleRemoteReboostOfRemotePost of {
      id: string;
      published: Ptime.t;
      target: string;
      author: string;
      raw_data: Yojson.Safe.t;
    }
  | SearchRemoteUser of {
      username: string;
      domain: string option;
      local_user: Database.LocalUser.t option;
    }

  (* post by local user *)
  | LocalPost of {
      user: Database.LocalUser.t;
      title: string option;
      content_type: [ `Markdown | `Org | `Text ];
      scope: [ `DM | `Followers | `Public ];
      post_to: string list option;
      content: string;
      in_reply_to: string option;
      attachments: (string * string) list;
    }
  | LocalLike of {
      user: Database.LocalUser.t;
      post: Database.Posts.t;
    }
  | LocalReboost of {
      user: Database.LocalUser.t;
      post: Database.Posts.t;
    }


let task_name = function
  | CreateRemoteNote _ -> "CreateRemoteNote"
  | HandleUndoFollow _ -> "HandleUndoFollow"
  | HandleUndo _ -> "HandleUndo"
  | HandleRemoteFollow _ -> "HandleRemoteFollow"
  | HandleAcceptFollow _ -> "HandleAcceptFollow"
  | FollowRemoteUser _ -> "FollowRemoteUser"
  | UnfollowRemoteUser _ -> "UnfollowRemoteUser"
  | HandleRemoteLikeOfLocalPost _ -> "HandleRemoteLikeOfLocalPost"
  | HandleRemoteLikeOfRemotePost _ -> "HandleRemoteLikeOfRemotePost"
  | HandleRemoteReboostOfLocalPost _ -> "HandleRemoteReboostOfLocalPost"
  | HandleRemoteReboostOfRemotePost _ -> "HandleRemoteReboostOfRemotePost"
  | SearchRemoteUser _ -> "SearchRemoteUser"
  | LocalPost _ -> "LocalPost"
  | LocalLike _ -> "LocalLike"
  | LocalReboost _ -> "LocalReboost"

