# pumpkin-server


For local development:

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
