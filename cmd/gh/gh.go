package main

import (
	"context"
	"fmt"
	"net/http"
	"strconv"
	"strings"

	"github.com/bradleyfalzon/ghinstallation"
	"github.com/google/go-github/v33/github"
	"golang.org/x/oauth2"
)

func getInstallationTransportFromEnv(ctx context.Context, env Env) (*ghinstallation.Transport, error) {
	key := []byte(env.Get("APP_KEY"))

	appID, err := strconv.ParseInt(env.Get("APP_ID"), 10, 64)
	if err != nil {
		return nil, err
	}

	githubRepository := env.Get("GITHUB_REPOSITORY")
	owner, repo, err := getOwnerRepo(githubRepository)
	if err != nil {
		return nil, err
	}

	token := env.Get("GITHUB_TOKEN")

	repoID, err := getRepoID(ctx, token, owner, repo)
	if err != nil {
		return nil, err
	}

	return getInstallationTransport(ctx, appID, key, repoID)
}

func getInstallationTransport(ctx context.Context, appID int64, key []byte, repoID int64) (*ghinstallation.Transport, error) { //nolint
	// The AppService requires JWT authentication, so we create a single-use
	// transport and client for that purpose.
	atr, err := ghinstallation.NewAppsTransport(http.DefaultTransport, appID, key)
	if err != nil {
		return nil, err
	}
	appClient := github.NewClient(&http.Client{Transport: atr})

	installation, _, err := appClient.Apps.FindRepositoryInstallationByID(ctx, repoID)
	if err != nil {
		return nil, err
	}
	installationID := installation.GetID()

	return ghinstallation.NewFromAppsTransport(atr, installationID), nil
}

func getOwnerRepo(repository string) (string, string, error) {
	expectedFieldCount := 2
	fields := strings.SplitN(repository, "/", expectedFieldCount)
	if len(fields) != expectedFieldCount {
		return "", "", fmt.Errorf("expected repository name like owner/name")
	}
	owner := fields[0]
	repo := fields[1]
	return owner, repo, nil
}

func getRepoID(ctx context.Context, token, owner, repo string) (int64, error) {
	ts := oauth2.StaticTokenSource(
		&oauth2.Token{AccessToken: token},
	)
	client := github.NewClient(oauth2.NewClient(ctx, ts))

	repository, _, err := client.Repositories.Get(ctx, owner, repo)
	if err != nil {
		return 0, err
	}
	return repository.GetID(), nil
}
