  $ ./decoder.exe create-note resources/pleroma-create-note.json
  { Types.id =
    "https://pleroma.ocamlot.xyz/activities/3c496b38-c303-47f9-b4b8-5eb484ecc6b3";
    actor = "https://pleroma.ocamlot.xyz/users/random";
    published = (Some 2023-01-27 15:18:38 +00:00);
    to_ =
    ["https://www.w3.org/ns/activitystreams#Public";
      "https://testing.ocamlot.xyz/users/orangemerin"];
    cc = ["https://pleroma.ocamlot.xyz/users/random/followers"];
    direct_message = false;
    obj =
    { Types.id =
      "https://pleroma.ocamlot.xyz/objects/ad4e1d78-8ceb-4697-88fe-ddbb6f42557e";
      actor = "https://pleroma.ocamlot.xyz/users/random";
      to_ =
      ["https://www.w3.org/ns/activitystreams#Public";
        "https://testing.ocamlot.xyz/users/orangemerin"];
      in_reply_to = None;
      cc = ["https://pleroma.ocamlot.xyz/users/random/followers"];
      content =
      "<span class=\"h-card\"><a class=\"u-url mention\" data-user=\"AS4V0TbTpJvpUJz5bk\" href=\"https://testing.ocamlot.xyz/users/orangemerin\" rel=\"ugc\">@<span>orangemerin</span></a></span> Hey buddy!";
      sensitive = false;
      source = (Some "@orangemerin@testing.ocamlot.xyz Hey buddy!");
      summary = (Some ""); published = (Some 2023-01-27 15:18:38 +00:00);
      tags =
      [`Value ({ Types.ty = `Mention;
                 href = "https://testing.ocamlot.xyz/users/orangemerin";
                 name = "@orangemerin@testing.ocamlot.xyz" })
        ];
      raw =
      {
        "actor": "https://pleroma.ocamlot.xyz/users/random",
        "attachment": [],
        "attributedTo": "https://pleroma.ocamlot.xyz/users/random",
        "cc": [ "https://pleroma.ocamlot.xyz/users/random/followers" ],
        "content": "<span class=\"h-card\"><a class=\"u-url mention\" data-user=\"AS4V0TbTpJvpUJz5bk\" href=\"https://testing.ocamlot.xyz/users/orangemerin\" rel=\"ugc\">@<span>orangemerin</span></a></span> Hey buddy!",
        "context": "https://pleroma.ocamlot.xyz/contexts/594a9be8-f164-451c-9442-a74354915e07",
        "conversation": "https://pleroma.ocamlot.xyz/contexts/594a9be8-f164-451c-9442-a74354915e07",
        "id": "https://pleroma.ocamlot.xyz/objects/ad4e1d78-8ceb-4697-88fe-ddbb6f42557e",
        "published": "2023-01-27T15:18:38.430120Z",
        "sensitive": null,
        "source": {
          "content": "@orangemerin@testing.ocamlot.xyz Hey buddy!",
          "mediaType": "text/plain"
        },
        "summary": "",
        "tag": [
          {
            "href": "https://testing.ocamlot.xyz/users/orangemerin",
            "name": "@orangemerin@testing.ocamlot.xyz",
            "type": "Mention"
          }
        ],
        "to": [
          "https://www.w3.org/ns/activitystreams#Public",
          "https://testing.ocamlot.xyz/users/orangemerin"
        ],
        "type": "Note"
      } };
    raw =
    {
      "@context": [
        "https://www.w3.org/ns/activitystreams",
        "https://pleroma.ocamlot.xyz/schemas/litepub-0.1.jsonld",
        { "@language": "und" }
      ],
      "actor": "https://pleroma.ocamlot.xyz/users/random",
      "cc": [ "https://pleroma.ocamlot.xyz/users/random/followers" ],
      "context": "https://pleroma.ocamlot.xyz/contexts/594a9be8-f164-451c-9442-a74354915e07",
      "directMessage": false,
      "id": "https://pleroma.ocamlot.xyz/activities/3c496b38-c303-47f9-b4b8-5eb484ecc6b3",
      "object": {
        "actor": "https://pleroma.ocamlot.xyz/users/random",
        "attachment": [],
        "attributedTo": "https://pleroma.ocamlot.xyz/users/random",
        "cc": [ "https://pleroma.ocamlot.xyz/users/random/followers" ],
        "content": "<span class=\"h-card\"><a class=\"u-url mention\" data-user=\"AS4V0TbTpJvpUJz5bk\" href=\"https://testing.ocamlot.xyz/users/orangemerin\" rel=\"ugc\">@<span>orangemerin</span></a></span> Hey buddy!",
        "context": "https://pleroma.ocamlot.xyz/contexts/594a9be8-f164-451c-9442-a74354915e07",
        "conversation": "https://pleroma.ocamlot.xyz/contexts/594a9be8-f164-451c-9442-a74354915e07",
        "id": "https://pleroma.ocamlot.xyz/objects/ad4e1d78-8ceb-4697-88fe-ddbb6f42557e",
        "published": "2023-01-27T15:18:38.430120Z",
        "sensitive": null,
        "source": {
          "content": "@orangemerin@testing.ocamlot.xyz Hey buddy!",
          "mediaType": "text/plain"
        },
        "summary": "",
        "tag": [
          {
            "href": "https://testing.ocamlot.xyz/users/orangemerin",
            "name": "@orangemerin@testing.ocamlot.xyz",
            "type": "Mention"
          }
        ],
        "to": [
          "https://www.w3.org/ns/activitystreams#Public",
          "https://testing.ocamlot.xyz/users/orangemerin"
        ],
        "type": "Note"
      },
      "published": "2023-01-27T15:18:38.429999Z",
      "to": [
        "https://www.w3.org/ns/activitystreams#Public",
        "https://testing.ocamlot.xyz/users/orangemerin"
      ],
      "type": "Create"
    } }
