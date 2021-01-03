package main

import (
	"context"
	"fmt"
	"io/ioutil"
	"os"

	"github.com/apex/log"
	"github.com/google/go-github/v33/github"
	"golang.org/x/mod/semver"
	"golang.org/x/oauth2"
)

func handlePush(ctx context.Context, client *github.Client, env Env, event *github.PushEvent) error {
	sha := env.Get("GITHUB_SHA")
	tags, err := listTags(env.Get("TAG_FILE"))
	if err != nil {
		return err
	}
	if len(tags) == 0 {
		return fmt.Errorf("found no tags")
	}
	sortTags(tags)
	latest := tags[len(tags)]

	log.WithField("latest", latest).Debug("validating latest tag")
	if !semver.IsValid(latest) {
		return fmt.Errorf("latest tag is not valid semver")
	}
	repo := event.GetRepo().GetName()
	owner := event.GetRepo().GetOwner().GetLogin()

	release, err := getReleaseForTag(ctx, client, owner, repo, latest)
	if err != nil {
		return err
	}

	body, err := genReleaseBody(ctx, client)
	if err != nil {
		return err
	}

	if release != nil {
		release.Body = github.String(body)
		_, _, err := client.Repositories.EditRelease(ctx, owner, repo, release.GetID(), release)
		return err
	}

	release = &github.RepositoryRelease{
		TagName:         github.String(latest),
		Name:            github.String(latest),
		TargetCommitish: github.String(sha),
		Draft:           github.Bool(true),
		Body:            github.String(body),
	}
	_, _, err = client.Repositories.CreateRelease(ctx, owner, repo, release)
	return err
}

func genReleaseBody(ctx context.Context, client *github.Client, owner, repo, tag string) (string, error) {
	body := ""
	comparison, _, err := client.Repositories.CompareCommits(ctx, owner, repo, tag, "HEAD")
	if err != nil {
		return "", err
	}
	for _, commit := range comparison.Commits {

	}
	return body, nil
}

func compareCommits(ctx context.Context, client *github.Client, owner, repo, base, head string) {
	comparison, resp, err := client.Repositories.CompareCommits(ctx, owner, repo, tag, "HEAD")
	var releases []*github.RepositoryRelease
	opt := &github.ListOptions{}
	for {
		got, resp, err := client.Repositories.ListReleases(ctx, ownerName, repoName, opt)
		if err != nil {
			return nil, err
		}
		releases = append(releases, got...)
		if resp.NextPage == 0 {
			break
		}
		opt.Page = resp.NextPage
	}
}

func getReleaseForTag(ctx context.Context, client *github.Client, owner, repo, tag string) (*github.RepositoryRelease, error) {
	release, resp, err := client.Repositories.GetReleaseByTag(ctx, owner, repo, tag)
	if err != nil {
		return nil, err
	}
	if resp.Response.StatusCode == 404 {
		return nil, nil
	}
	return release, nil
}

func createOrUpdateRelease(env Env) error {
	token := env.Get("GITHUB_TOKEN")
	eventPath := env.Get("GITHUB_EVENT_PATH")
	eventName := env.Get("GITHUB_EVENT_NAME")

	ctx := context.Background()
	ts := oauth2.StaticTokenSource(
		&oauth2.Token{AccessToken: token},
	)
	tc := oauth2.NewClient(ctx, ts)
	client := github.NewClient(tc)

	payload, err := ioutil.ReadFile(eventPath)
	if err != nil {
		return err
	}

	event, err := github.ParseWebHook(eventName, payload)
	if err != nil {
		return err
	}

	if level == log.DebugLevel {
		dumpEnvironment(os.Stderr, os.Environ())
		dumpPayload(os.Stderr, payload)
	}

	switch e := event.(type) {
	case *github.PushEvent:
		return handlePush(ctx, client, env, e)
	default:
		log.Debug("handle unknown event type")
		return nil
	}
	return nil
}

// scratch

func updateDraftRelease(ctx context.Context, env Env, release *github.RepositoryRelease) error {
	return nil
}

func createDraftRelease(ctx, env, release)

func createOrUpdateRelease2(env Env) error {
	token := env.Get("GITHUB_TOKEN")
	eventPath := env.Get("GITHUB_EVENT_PATH")
	eventName := env.Get("GITHUB_EVENT_NAME")

	ctx := context.Background()
	ts := oauth2.StaticTokenSource(
		&oauth2.Token{AccessToken: token},
	)
	tc := oauth2.NewClient(ctx, ts)
	client := github.NewClient(tc)

	payload, err := ioutil.ReadFile(eventPath)
	if err != nil {
		return err
	}

	event, err := github.ParseWebHook(eventName, payload)
	if err != nil {
		return err
	}

	if level == log.DebugLevel {
		dumpEnvironment(os.Stderr, os.Environ())
		dumpPayload(os.Stderr, payload)
	}

	switch e := event.(type) {
	case *github.PushEvent:
		return handlePush(ctx, client, env, e)
	default:
		log.Debug("handle unknown event type")
		return nil
	}
}

func getLatestRelease(ctx context.Context, client *github.Client, event *github.PushEvent) (*github.RepositoryRelease, error) {
	repo := event.GetRepo()
	owner := repo.GetOwner()
	ownerName := owner.GetLogin()
	repoName := repo.GetName()

	log.WithFields(&log.Fields{
		"repo":  repo,
		"owner": ownerName,
	}).Debug("collecting releases")
	var releases []*github.RepositoryRelease
	opt := &github.ListOptions{}
	for {
		got, resp, err := client.Repositories.ListReleases(ctx, ownerName, repoName, opt)
		if err != nil {
			return nil, err
		}
		releases = append(releases, got...)
		if resp.NextPage == 0 {
			break
		}
		opt.Page = resp.NextPage
	}

	log.WithField("releases", len(releases)).Debug("sorting releases")
	sort.slice(releases, func(i, j int) bool {
		return semver.compare(releases[i].gettagname(), releases[j].gettagname()) == -1
	})

	release := releases[len(releases)]
	log.WithField("release", release).Debug("got current release")
	return release, nil
}
