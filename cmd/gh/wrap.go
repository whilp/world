package main

import (
	"context"
	"fmt"
	"os"
	"os/exec"

	"github.com/apex/log"
)

func wrap(env Env) error {
	ctx := context.Background()

	minArgLen := 2
	if len(os.Args) < minArgLen {
		return fmt.Errorf("expected at least %d arguments (including argv[0])", minArgLen)
	}
	args := os.Args[1:]

	wrapTokenName := env.Get("WRAP_TOKEN_NAME")
	wrapTokenPrefix := env.Get("WRAP_TOKEN_PREFIX")

	log.WithFields(&log.Fields{
		"WRAP_TOKEN_NAME":   wrapTokenName,
		"WRAP_TOKEN_PREFIX": wrapTokenPrefix,
	}).Debug("acquiring access token")

	tr, err := getInstallationTransportFromEnv(ctx, env)
	if err != nil {
		return err
	}

	accessToken, err := tr.Token(ctx)
	if err != nil {
		return err
	}

	unsafeVariables := []string{
		"GITHUB_TOKEN",
		"APP_ID",
		"APP_KEY",
	}

	log.WithField("vars", unsafeVariables).Debug("sanitizing sensitive environment variables")

	for _, v := range unsafeVariables {
		if err := os.Unsetenv(v); err != nil {
			return err
		}
	}

	if err := os.Setenv(wrapTokenName, wrapTokenPrefix+accessToken); err != nil {
		return err
	}

	log.WithFields(&log.Fields{
		"ARGS": args,
	}).Debug("running wrapped entrypoint")
	cmd := exec.Command(args[0], args[1:]...) //nolint
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	cmd.Env = os.Environ()
	return cmd.Run()
}
