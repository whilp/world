package main

import (
	"context"
	"fmt"
	"strings"

	"github.com/apex/log"
	"github.com/google/go-github/v32/github"
)

func handlePullRequest(ctx context.Context, client *github.Client, env Env, event *github.PullRequestEvent) error {
	repo := event.GetRepo()
	owner := repo.GetOwner()
	ownerName := owner.GetLogin()
	repoName := repo.GetName()
	sha := event.PullRequest.Head.GetSHA()
	statusContext := getStatusContext(env)
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

func getStatusContext(env Env) string {
	statusContext, ok := env["CONTEXT"]
	if !ok {
		statusContext = "lint / github"
	}
	return statusContext
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
	serverURL := env["GITHUB_SERVER_URL"]
	repository := env["GITHUB_REPOSITORY"]
	runID := env["GITHUB_RUN_ID"]
	return fmt.Sprintf("%s/%s/actions/runs/%s", serverURL, repository, runID)
}
