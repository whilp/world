package main

// TODO: walk to find repo.json
// TODO: check for updates

import (
	"encoding/json"
	"flag"
	"io/ioutil"
	"log"
	"os"
	"path"
)

var fInput = flag.String("input", "", "path to input .json")
var fOutput = flag.String("output", "", "path to output .bzl")
var fPkg = flag.String("pkg", "", "path to package containing repo.json")

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

	inf := *fInput
	outf := *fOutput
	if fPkg != nil {
		inf = path.Join(*fPkg, "repo.json")
		outf = path.Join(*fPkg, "repo.bzl")
	}

	input, err := ioutil.ReadFile(inf)
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

	out, err := os.Create(outf)
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
