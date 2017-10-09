# pumpkin-server

Haskell API server and occurrence matcher, which provide a
self-contained alternative to the Pumpkin Rails app. The key missing
feature w.r.t. the Rails app is Google auth.

## Docker build

These apps can be built as slim Docker images based on Alpine. Simply
run

```
make docker
```

and wait a little while.

An example stack can then be started with docker compose using the
provided `docker/docker-compose.yml`. The web server will be exposed
on `localhost`'s port 8080.

Note that the database must currently be set up manually, e.g. by
loading `PROJECTROOT/db/structure.sql` into it: see the compose file
for connection details.

## For local development

```
stack build

# Use the standard PostgreSQL env vars to configure the DB connection, e.g.
export PGDATABASE=pumpkin_development

stack exec -- pumpkin-server --rootdir ../public
```

API requests will then be served on 8080, while EKG metrics will be visible at port 8000.

Run the matcher with:
```
stack exec -- pumpkin-matcher
```

Use `--help` with these commands to show command line options.
