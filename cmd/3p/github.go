package main

import "fmt"

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

// TODO: would be nice to support multiple URLs.
func (g githubArchive) url() string {
	return fmt.Sprintf("https://github.com/%s/%s/archive/%s.tar.gz", g.Owner, g.repo(), g.Ref)
}

func (g githubArchive) stripPrefix() string {
	return fmt.Sprintf("%s-%s", g.repo(), g.Ref)
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

func githubURL(owner, repo, ref string) string {
	return fmt.Sprintf("https://github.com/%s/%s/archive/%s.tar.gz", owner, repo, ref)
}

func githubPrefix(repo, ref string) string {
	return fmt.Sprintf("%s-%s", repo, ref)
}
