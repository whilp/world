package main

import (
	"bytes"
	"flag"
	"io/ioutil"
	"log"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
)

var (
	fShfmt      = flag.String("shfmt", "", "path to shfmt executable")
	fShellcheck = flag.String("shellcheck", "", "path to shellcheck executable")
)

func main() {
	err := run()
	if err != nil {
		log.Fatal(err)
	}
}

func run() error {
	flag.Parse()

	shfmt, err := filepath.Abs(*fShfmt)
	if err != nil {
		return err
	}

	shellcheck, err := filepath.Abs(*fShellcheck)
	if err != nil {
		return err
	}

	buildWorkspaceDirectory := os.Getenv("BUILD_WORKSPACE_DIRECTORY")
	if buildWorkspaceDirectory == "" {
		buildWorkspaceDirectory = "."
	}

	err = os.Chdir(buildWorkspaceDirectory)
	if err != nil {
		return err
	}

	manifest := os.Getenv("MANIFEST")
	paths, err := getPaths(manifest)
	if err != nil {
		return err
	}

	shellPaths, err := getShellPaths(shfmt, paths)
	if err != nil {
		return err
	}

	return runShellcheck(shellcheck, flag.Args(), shellPaths)
}

func runShellcheck(shellcheck string, args, paths []string) error {
	args = append(args, paths...)
	cmd := exec.Command(shellcheck, args...)

	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr

	return cmd.Run()
}

func getShellPaths(shfmt string, paths []string) ([]string, error) {
	args := append([]string{"-f"}, paths...)
	cmd := exec.Command(shfmt, args...)

	var stdout strings.Builder
	cmd.Stdout = &stdout
	cmd.Stderr = os.Stderr

	err := cmd.Run()
	if err != nil {
		return nil, err
	}
	return validStrings(strings.Split(stdout.String(), "\n")), nil
}

func getPaths(manifest string) ([]string, error) {
	var (
		contents []byte
		err      error
	)
	switch manifest {
	case "":
		contents, err = gitLsFiles()
	default:
		contents, err = ioutil.ReadFile(manifest)
	}
	if err != nil {
		return nil, err
	}

	return validStrings(strings.Split(string(contents), "\x00")), nil
}

func validStrings(candidates []string) []string {
	got := []string{}
	for _, candidate := range candidates {
		if candidate != "" {
			got = append(got, candidate)
		}
	}

	return got
}

func gitLsFiles() ([]byte, error) {
	args := []string{"ls-files", "-z"}
	cmd := exec.Command("git", args...)

	var stdout bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = os.Stderr

	err := cmd.Run()
	if err != nil {
		return nil, err
	}
	return stdout.Bytes(), nil
}
