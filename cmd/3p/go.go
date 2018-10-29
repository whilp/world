package main

import "fmt"

type goRepository struct {
	Name   string `json:"name"`
	Owner  string `json:"owner"`
	Ref    string `json:"ref"`
	Repo   string `json:"repo"`
	Sha256 string `json:"sha256"`
}

func (g goRepository) ruleName() string {
	return g.Name
}

func (g goRepository) repo() string {
	if g.Repo == "" {
		return g.Name
	}
	return g.Repo
}

func (g goRepository) importPath() string {
	return fmt.Sprintf("github.com/%s/%s", g.Owner, g.repo())
}

func (g goRepository) Rule() rule {
	return rule{
		Name: "go_repository",
		Load: "@bazel_gazelle//:deps.bzl",
		URLs: []string{
			githubURL(g.Owner, g.repo(), g.Ref),
		},
		Parameters: map[string]string{
			"name":         g.ruleName(),
			"strip_prefix": githubPrefix(g.repo(), g.Ref),
			"importpath":   g.importPath(),
			"sha256":       g.Sha256,
		},
	}
}
