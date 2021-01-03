package main

import (
	"fmt"
	"os"

	"github.com/apex/log"
	"github.com/apex/log/handlers/cli"
)

var level = log.DebugLevel

type dispatcher map[string]func(Env) error

func main() {
	if err := run(); err != nil {
		log.WithError(err).Error("failed")
		os.Exit(1)
	}
}

func run() error {
	env := NewEnv(os.Environ())

	log.SetLevel(level)
	log.SetHandler(cli.New(os.Stderr))

	dispatch := dispatcher{
		"check":          check,
		"check-pr":       checkPr,
		"auth":           auth,
		"wrap":           wrap,
		"update-release": createOrUpdateRelease,
	}

	rawCommand := env.Get("COMMAND")
	command, ok := dispatch[rawCommand]
	if !ok {
		return fmt.Errorf("unexpected command: %v", rawCommand)
	}

	return command(env)
}

func check(env Env) error {
	log.Debug("we're ok!")
	return nil
}
