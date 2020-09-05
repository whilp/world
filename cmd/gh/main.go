package main

import (
	"context"
	"io/ioutil"
	"os"

	"github.com/apex/log"
	"github.com/apex/log/handlers/cli"
	"github.com/google/go-github/v32/github"
	"golang.org/x/oauth2"
)

var level = log.DebugLevel

func main() {
	if err := run(); err != nil {
		log.WithError(err).Error("failed")
		os.Exit(1)
	}
}

func run() error {
	log.SetLevel(level)
	log.SetHandler(cli.New(os.Stderr))

	if os.Getenv("CHECK") != "" {
		log.Debug("we're ok!")
		return nil
	}

	token := os.Getenv("GITHUB_TOKEN")
	eventPath := os.Getenv("GITHUB_EVENT_PATH")
	eventName := os.Getenv("GITHUB_EVENT_NAME")

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

	env := environ()
	switch e := event.(type) {
	case *github.PullRequestEvent:
		return handlePullRequest(ctx, client, env, e)
	default:
		log.Debug("handle unknown event type")
		return nil
	}
}
