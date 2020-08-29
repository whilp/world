package main

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"sort"
	"strings"

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
	statusContext := os.Getenv("CONTEXT")
	if statusContext == "" {
		statusContext = "lint / github"
	}

	ctx := context.Background()
	ts := oauth2.StaticTokenSource(
		&oauth2.Token{AccessToken: token},
	)
	tc := oauth2.NewClient(ctx, ts)
	client := github.NewClient(tc)

	debugEnvironment(os.Environ())

	labels := getLabels(os.Environ())

	log.WithField("EventPath", eventPath).Debug("read payload")
	payload, err := ioutil.ReadFile(eventPath)
	if err != nil {
		return err
	}

	debugPayload(payload)

	h := &handler{ctx, client, statusContext, labels}
	log.WithField("EventName", eventName).Debug("parse payload")
	event, err := github.ParseWebHook(eventName, payload)
	if err != nil {
		return err
	}

	switch event := event.(type) {
	case *github.PullRequestEvent:
		return h.handlePullRequest(event)
	}

	log.WithField("EventType", "unknown").Debug("unknown event type")
	return nil
}

type handler struct {
	ctx           context.Context
	client        *github.Client
	statusContext string
	labels        map[string]bool
}

func (h *handler) handlePullRequest(event *github.PullRequestEvent) error {
	repo := event.GetRepo()
	owner := repo.GetOwner()
	ownerName := owner.GetLogin()
	repoName := repo.GetName()
	sha := event.PullRequest.Head.GetSHA()

	status := &github.RepoStatus{
		Context: github.String(h.statusContext),
	}

	label, ok := hasAcceptableLabel(h.labels, event.PullRequest)
	if ok {
		status.State = github.String("success")
		status.Description = github.String(fmt.Sprintf("Found acceptable label: %v", label))
	} else {
		status.State = github.String("failure")
		status.Description = github.String("No acceptable labels")
	}

	log.WithFields(&log.Fields{
		"Handler": "PullRequest",
		"Repo":    repoName,
		"Owner":   ownerName,
		"SHA":     sha,
		"Label":   label,
		"OK":      ok,
	}).Debug("create status")

	_, _, err := h.client.Repositories.CreateStatus(h.ctx, ownerName, repoName, sha, status)
	if err != nil {
		return err
	}
	return nil
}

func getLabels(vars []string) map[string]bool {
	labels := make(map[string]bool)
	for _, v := range vars {
		if !strings.HasPrefix(v, "LABEL_") {
			continue
		}

		fields := strings.Split(v, "=")
		if len(fields) != 2 {
			continue
		}
		labels[fields[1]] = true
	}
	return labels
}

func hasAcceptableLabel(labels map[string]bool, pr *github.PullRequest) (string, bool) {
	for _, l := range pr.Labels {
		label := l.GetName()
		log.WithField("Label", label).Debug("check label")
		_, acceptable := labels[label]
		if acceptable {
			return label, acceptable
		}
	}
	return "", false
}

func debugEnvironment(env []string) {
	if level != log.DebugLevel {
		return
	}

	group := &Group{"environment", &bytes.Buffer{}}

	sort.Strings(env)
	for _, e := range env {
		io.WriteString(group, fmt.Sprintln(e))
	}
	group.Dump(os.Stderr)
}

func debugPayload(src []byte) {
	if level != log.DebugLevel {
		return
	}

	var buf bytes.Buffer
	json.Indent(&buf, src, "", " ")
	group := &Group{"payload", &buf}
	group.Dump(os.Stderr)
}

// Group is
type Group struct {
	name string
	*bytes.Buffer
}

// Dump does
func (g Group) Dump(out io.Writer) error {
	if _, err := io.WriteString(out, fmt.Sprintf("::group::%s\n", g.name)); err != nil {
		return err
	}

	if _, err := out.Write(g.Bytes()); err != nil {
		return err
	}

	if _, err := io.WriteString(out, "\n::endgroup::\n"); err != nil {
		return err
	}

	return nil
}
