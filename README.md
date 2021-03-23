# activitypub-server

[![Actions Status](https://github.com/gopiandcode/activitypub-server/workflows/CI/badge.svg)](https://github.com/gopiandcode/activitypub-server/actions)

A simple single user activitypub federated server instance

## Project structure

```
.
|-- logs
|-- public
|-- resources
|-- test
|   `-- test.ml
|-- app                                (* Core Logic of Application *)
|   |-- command                        (* CLI Commands to interact with the application database *)
|   |-- context                        (* Core logic of project *)
|   `-- schedule                       (* Schedules/chron jobs that should be run periodically *)
|-- database
|   `-- migration.ml
|-- routes
|   |-- global.ml                      (* Defines global definitions used by frontend and back  *)
|   |-- api.ml                         (* Defines backend server API *)
|   `-- site.ml                        (* Defines frontend of server *)
|-- run
|   `-- run.ml                         (* Main entrypoint of server *)
|-- service
|   `-- service.ml                     (* Services of the application *)
`-- web
    |-- handler                        (* Defines request handlers *)
    |-- middleware                     (* Custom Request Middlewares *)
    `-- view                           (* Frontend sites *)

40 directories, 124 files
```


## Contributing

Take a look at our [Contributing Guide](CONTRIBUTING.md).
