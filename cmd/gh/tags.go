package main

import (
	"bytes"
	"io/ioutil"
	"os"
	"os/exec"
	"sort"

	"golang.org/x/mod/semver"
)

func sortTags(tags []string) {
	sort.Slice(tags, func(i, j int) bool {
		return semver.Compare(tags[i], tags[j]) == -1
	})
}

func listTags(tagFile string) ([]string, error) {
	var (
		raw []byte
		err error
	)

	if tagFile != "" {
		raw, err = ioutil.ReadFile(tagFile)
	} else {
		raw, err = listTagsGit()
	}
	if err != nil {
		return []string{}, err
	}
	return splitTags(raw), nil
}

func splitTags(raw []byte) []string {
	tags := []string{}
	chunks := bytes.Split(raw, []byte("\n"))
	for _, chunk := range chunks {
		tags = append(tags, string(chunk))
	}
	return tags
}

func listTagsGit() ([]byte, error) {
	cmd := exec.Command("git", "tag")
	cmd.Stderr = os.Stderr
	out, err := cmd.Output()
	return out, err
}
