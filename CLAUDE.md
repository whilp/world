## Build workflow

```bash
make test              # run tests (incremental)
make clean test        # full rebuild and test
make test; make test   # verify incremental builds work (second should be instant)
make staged            # fetch and extract dependencies
make fetched           # fetch dependencies only
```
