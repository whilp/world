package main

import (
	"context"
	"fmt"
	"io/ioutil"
	"os"
	"strings"

	"github.com/apex/log"
	"github.com/google/go-github/v33/github"
	"golang.org/x/oauth2"
)

func checkPr(env Env) error {
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
	case *github.PullRequestEvent:
		return handlePullRequest(ctx, client, env, e)
	default:
		log.Debug("handle unknown event type")
		return nil
	}
}

func handlePullRequest(ctx context.Context, client *github.Client, env Env, event *github.PullRequestEvent) error {
	repo := event.GetRepo()
	owner := repo.GetOwner()
	ownerName := owner.GetLogin()
	repoName := repo.GetName()
	sha := event.PullRequest.Head.GetSHA()
	statusContext := env.Get("CONTEXT", "lint / github")
	runURL := getRunURL(env)

	status := &github.RepoStatus{
		Context:   github.String(statusContext),
		State:     github.String("failure"),
		TargetURL: github.String(runURL),
	}

	configured := getLabels(env)
	allowed := []string{}
	other := []string{}
	for _, l := range event.PullRequest.Labels {
		label := l.GetName()
		if _, isConfigured := configured[label]; isConfigured {
			allowed = append(allowed, label)
		} else {
			other = append(other, label)
		}
	}

	configuredLabels := []string{}
	for l := range configured {
		configuredLabels = append(configuredLabels, l)
	}

	switch len(allowed) {
	case 1:
		status.State = github.String("success")
		status.Description = github.String(fmt.Sprintf("Found one allowed label: %v", allowed[0]))
	case 0:
		status.Description = github.String("No labels; apply one allowed label")
	default:
		status.Description = github.String("Too many allowed labels; choose one")
	}

	log.WithFields(&log.Fields{
		"ConfiguredLabels": configuredLabels,
		"AllowedLabels":    allowed,
		"OtherLabels":      other,
		"Repo":             repoName,
		"Owner":            ownerName,
		"SHA":              sha,
	}).Debug("handle pull request")

	_, _, err := client.Repositories.CreateStatus(ctx, ownerName, repoName, sha, status)
	return err
}

func getLabels(env Env) map[string]bool {
	labels := make(map[string]bool)
	for k, v := range env {
		if strings.HasPrefix(strings.ToUpper(k), "LABEL_") {
			labels[v] = true
		}
	}
	return labels
}

func getRunURL(env Env) string {
	serverURL := env.Get("GITHUB_SERVER_URL")
	repository := env.Get("GITHUB_REPOSITORY")
	runID := env.Get("GITHUB_RUN_ID")
	return fmt.Sprintf("%s/%s/actions/runs/%s", serverURL, repository, runID)
}
