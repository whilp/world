package main

// TODO: walk to find repo.json
// TODO: check for updates

import (
	"encoding/json"
	"flag"
	"fmt"
	"io"
	"io/ioutil"
	"log"
	"os"
	"text/template"
)

var fInput = flag.String("input", "", "path to input .json")
var fOutput = flag.String("output", "", "path to output .bzl")

func main() {
	// read repo.json
	// write repo.bzl
	if err := run(); err != nil {
		log.Fatal(err)
	}
}

func run() error {
	var r repo

	flag.Parse()

	input, err := ioutil.ReadFile(*fInput)
	if err != nil {
		return err
	}

	err = json.Unmarshal(input, &r)
	if err != nil {
		return err
	}

	b := bazel{}
	for _, x := range r.GithubArchive {
		b.Add(x)
	}

	out, err := os.Create(*fOutput)
	if err != nil {
		return err
	}
	defer out.Close()

	b.Render(out)

	return nil
}

type repo struct {
	// TODO: do something with this.
	Version       string          `json:"version"`
	GithubArchive []githubArchive `json:"github_archive"`
}

type githubArchive struct {
	Name   string `json:"name"`
	Owner  string `json:"owner"`
	Ref    string `json:"ref"`
	Repo   string `json:"repo"`
	Sha256 string `json:"sha256"`
}

func (g githubArchive) ruleName() string {
	if g.Repo == "" {
		return g.Name
	}
	return fmt.Sprintf("github_%s_%s", g.Owner, g.Name)
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
		Parameters: map[string]string{
			"name":         g.ruleName(),
			"url":          g.url(),
			"strip_prefix": g.stripPrefix(),
		},
	}
}

type bazel struct {
	rules []rule
}

func (b *bazel) Render(out io.Writer) {
	var t = template.Must(template.New("name").Parse(tmpl))
	t.Execute(out, b)
}

func (b *bazel) Rules() []rule {
	return b.rules
}

func (b *bazel) Add(r ruler) {
	b.rules = append(b.rules, r.Rule())
}

type ruler interface {
	Rule() rule
}

type rule struct {
	Name       string
	Parameters map[string]string
}

var tmpl = `
load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

def repo():
{{range $rule := .Rules}}
    {{$rule.Name}}(
    {{range $key, $val := .Parameters}}    {{$key}} = {{printf "%q" $val}},
    {{end}})
{{end}}
`
