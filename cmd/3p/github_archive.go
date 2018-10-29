package main

type githubArchive struct {
	Name   string `json:"name"`
	Owner  string `json:"owner"`
	Ref    string `json:"ref"`
	Repo   string `json:"repo"`
	Sha256 string `json:"sha256"`
}

func (g githubArchive) ruleName() string {
	return g.Name
}

func (g githubArchive) repo() string {
	if g.Repo == "" {
		return g.Name
	}
	return g.Repo
}

func (g githubArchive) Rule() rule {
	return rule{
		Name: "http_archive",
		Load: "@bazel_tools//tools/build_defs/repo:http.bzl",
		URLs: []string{
			githubURL(g.Owner, g.repo(), g.Ref),
		},
		Parameters: map[string]string{
			"name":         g.ruleName(),
			"strip_prefix": githubPrefix(g.repo(), g.Ref),
			"sha256":       g.Sha256,
		},
	}
}
