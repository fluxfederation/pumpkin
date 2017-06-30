# pumpkin-server


For local development:

```
stack build
export PGDATABASE=pumpkin_development
stack exec -- pumpkin-server --rootdir ../public
```

Run the matcher with:
```
stack exec -- pumpkin-matcher
```

API requests will then be served on 8080, while EKG metrics will be visible at port 8000.
