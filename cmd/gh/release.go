package main

import (
	"bytes"
	"context"
	"fmt"
	"text/template"
	"path"

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
	
	templateGlob := env.Get("TEMPLATE_GLOB")
	log.WithField("templateGlob", templateGlob).Debug("parsing templates")
	tmpl, err := template.ParseGlob(templateGlob)
	if err != nil {
		return err
	}

	latest, err := getLatestTag(env.Get("TAG_FILE"))

	ctx := context.Background()
	ts := oauth2.StaticTokenSource(
		&oauth2.Token{AccessToken: token},
	)
	tc := oauth2.NewClient(ctx, ts)
	client := github.NewClient(tc)

	log.WithFields(&log.Fields{
		"latest": latest,
		"sha":    sha,
	}).Debug("comparing commits")

	comparison, _, err := client.Repositories.CompareCommits(ctx, owner, repo, latest, sha)
	if err != nil {
		return err
	}
	
	body := &Body{
		Comparison: comparison,
		Latest: latest,
		SHA: sha,
	}
	
	log.Debug("drafting release body")
	release := &github.RepositoryRelease{}
	if err = draftRelease(release, tmpl, body); err != nil {
		return err
	}
	
	fields := &log.Fields{
		"sha": sha,
		"latest": latest,
		"commits": comparison.GetTotalCommits(),
	}

	notFound := 404
	got, resp, err := client.Repositories.GetReleaseByTag(ctx, owner, repo, latest)
	if err == nil {
		id := got.GetID()
		log.WithFields(fields).WithField("release", id).Debug("editing release")
		_, _, err = client.Repositories.EditRelease(ctx, owner, repo, id, release)
	} else if resp.StatusCode == notFound {
		log.WithFields(fields).Debug("creating release")
		_, _, err = client.Repositories.CreateRelease(ctx, owner, repo, release)
	}
	return err
}

type Body struct {
	Comparison *github.CommitsComparison
	Latest string
	SHA string
}

func draftRelease(release *github.RepositoryRelease, tmpl *template.Template, body *Body) error {
	release.Name = github.String(body.Latest)
	release.TagName = github.String(body.Latest)
	release.TargetCommitish = github.String(body.SHA)
	release.Draft = github.Bool(true)
	
	var b bytes.Buffer
	err := tmpl.Execute(&b, body)
	if err != nil {
		return err
	}
	release.Body = github.String(b.String())
	return nil
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
