## Build

```bash
make          # build everything
make clean    # remove all outputs
```

## Output structure

All outputs go under `o/`:

```
o/dl/   - downloaded files
o/bin/  - executable binaries
```

## Makefile conventions

- `o := o` defines the output root
- Pattern rules map `o/bin/%` from `o/dl/%`
- Downloads are checksum-verified with sha256sum
