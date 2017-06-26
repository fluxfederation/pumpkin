# pumpkin-server


For local development:

```
stack build
export PGDATABASE=pumpkin_development
stack exec pumpkin-server ../public 8080 8000
```

API requests will then be served on 8080, while EKG metrics will be visible at port 8000.
