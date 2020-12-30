// +build tools

//nolint
package main

// https://github.com/golang/go/issues/25922#issuecomment-414677877
// This file gets removed before lint runs (and then restored after) because Go
// will fail to analyze these main imports.
import (
	_ "github.com/ramya-rao-a/go-outline"
	_ "github.com/rogpeppe/godef"
	_ "github.com/stamblerre/gocode"
	_ "github.com/uudashr/gopkgs/v2/cmd/gopkgs"
	_ "go.starlark.net/cmd/starlark"
	_ "golang.org/x/lint/golint"
	_ "golang.org/x/tools/cmd/goimports"
	_ "golang.org/x/tools/cmd/guru"
)
