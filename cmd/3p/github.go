package main

import "fmt"

func githubURL(owner, repo, ref string) string {
	return fmt.Sprintf("https://github.com/%s/%s/archive/%s.tar.gz", owner, repo, ref)
}

func githubPrefix(repo, ref string) string {
	return fmt.Sprintf("%s-%s", repo, ref)
}
