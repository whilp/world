package main

type githubTar struct {
	Name   string `json:"name"`
	Owner  string `json:"owner"`
	Ref    string `json:"ref"`
	Repo   string `json:"repo"`
	Sha256 string `json:"sha256"`
}

func (g githubTar) ruleName() string {
	return g.Name
}

func (g githubTar) buildFileContent() string {
	return "XXX"
}

func (g githubTar) repo() string {
	if g.Repo == "" {
		return g.Name
	}
	return g.Repo
}

func (g githubTar) Rule() rule {
	return rule{
		Name: "http_archive",
		Load: "@bazel_tools//tools/build_defs/repo:http.bzl",
		URLs: []string{
			githubURL(g.Owner, g.repo(), g.Ref),
		},
		BuildFileContent: g.buildFileContent(),
		Parameters: map[string]string{
			"name":   g.ruleName(),
			"sha256": g.Sha256,
		},
	}
}
