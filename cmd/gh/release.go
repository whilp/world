package main

import (
	"context"
	"fmt"

	"github.com/apex/log"
	"github.com/google/go-github/v33/github"
	"golang.org/x/mod/semver"
	"golang.org/x/oauth2"
)

func updateRelease(env Env) error {
	token := env.Get("GITHUB_TOKEN")
	sha := env.Get("GITHUB_SHA")
	owner, repo, err := getOwnerRepo(env.Get("GITHUB_REPOSITORY"))
	if err != nil {
		return err
	}
	log.WithFields(&log.Fields{
		"command": "update-release",
		"owner":   owner,
		"repo":    repo,
	}).Debug("running command")

	latest, err := getLatestTag(env.Get("TAG_FILE"))
	log.WithFields(&log.Fields{
		"latest": latest,
		"sha":    sha,
	}).Debug("comparing commits")

	ctx := context.Background()
	ts := oauth2.StaticTokenSource(
		&oauth2.Token{AccessToken: token},
	)
	tc := oauth2.NewClient(ctx, ts)
	client := github.NewClient(tc)

	comparison, _, err := client.Repositories.CompareCommits(ctx, owner, repo, latest, sha)
	if err != nil {
		return err
	}

	notFound := 404
	shouldEdit := false
	release, resp, err := client.Repositories.GetReleaseByTag(ctx, owner, repo, latest)
	if err != nil {
		shouldEdit = resp.StatusCode != notFound
		if !shouldCreate {
			return err
		}
		release = &github.RepositoryRelease{}
	}

	draftRelease(release, comparison, latest, sha)

	log.WithFields(&log.Fields{
		"create": shouldCreate,
		"bytes":  len(release.GetBody()),
		"tag":    latest,
		"sha":    sha,
	}).Debug("creating or updating release")

	if shouldCreate {
		_, _, err = client.Repositories.CreateRelease(ctx, owner, repo, release)
	} else {
		_, _, err = client.Repositories.EditRelease(ctx, owner, repo, release.GetID(), release)
	}
	return err
}

func draftRelease(release *github.RepositoryRelease, comparison *github.CommitsComparison, latest, sha string) {
	body := fmt.Sprintf("%d commits!\n", comparison.GetTotalCommits())
	release.Name = github.String(latest)
	release.TagName = github.String(latest)
	release.TargetCommitish = github.String(sha)
	release.Body = github.String(body)
	release.Draft = github.Bool(true)
}

func getLatestTag(tagFile string) (string, error) {
	tags, err := listTags(tagFile)
	if err != nil {
		return "", err
	}
	if len(tags) == 0 {
		return "", fmt.Errorf("found no tags")
	}
	sortTags(tags)
	latest := tags[len(tags)-1]
	if !semver.IsValid(latest) {
		return "", fmt.Errorf("latest tag is not valid semver")
	}
	return latest, nil
}
