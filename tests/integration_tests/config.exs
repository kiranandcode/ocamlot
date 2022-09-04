# Pleroma instance configuration

# NOTE: This file should not be committed to a repo or otherwise made public
# without removing sensitive information.
# I DO WHAT I WANT

import Config

config :pleroma, Pleroma.Web.Endpoint,
   url: [host: "pleroma.ocamlot.xyz", scheme: "https", port: 443],
   http: [ip: {127, 0, 0, 1}, port: 4000],
   secret_key_base: "6pAz23tVjrU4XNTO34KDeuIhSwWRo1nl1ML4d15XRr+1DFtC98Id9do7wf4u3nzA",
   signing_salt: "ZZxo9Cl5"

config :pleroma, :instance,
  name: "pleroma.ocamlot.xyz",
  email: "johnny@go-die-in-a-whole",
  notify_email: "johnny",
  limit: 5000,
  registrations_open: true

config :pleroma, :media_proxy,
  enabled: false,
  redirect_on_failure: true
  #base_url: "https://cache.pleroma.social"

config :pleroma, Pleroma.Repo,
  adapter: Ecto.Adapters.Postgres,
  username: "pleroma",
  password: "HQrHnuWVff27td35zhPW4ZTwZe6CSDZBZgPIEvV6/jvhdCDNh7mZeJ2X3tAJ1Oa7",
  database: "pleroma",
  hostname: "postgres"

# Configure web push notifications
config :web_push_encryption, :vapid_details,
  subject: "mailto:johnny",
  public_key: "BJeoXzQ8a0K4iy8sCnTDWhNor5F_3N-bQCulpxwErQnNIG6pf9LIggMXmr2krhZfD7uzmd73s8ZrkG_3cZQqbXI",
  private_key: "cKm0zGXg4fScFglcmK1eHKNBV0tIT3i4HRsTMXYAX30"

config :pleroma, :database, rum_enabled: false
config :pleroma, :instance, static_dir: "/var/lib/pleroma/static"
config :pleroma, Pleroma.Uploaders.Local, uploads: "/var/lib/pleroma/uploads"

config :joken, default_signer: "Qb9DYFd75NbRk/DauhgZhpGfl0q2OxUjm97bpshL54Kq1Gs6LqQIrE95qyCNpY1Y"

config :pleroma, configurable_from_database: true

config :pleroma, Pleroma.Upload, filters: [Pleroma.Upload.Filter.Exiftool]
