# release: default to stable releases

Change release workflow to create stable releases by default instead of prereleases.

- .github/workflows/release.yml - set PRERELEASE to false

Stable releases are automatically marked as "latest" by GitHub, fixing the
`/releases/latest/download/home` URL used in the bootstrap command.

## Validation

- [x] workflow syntax valid
