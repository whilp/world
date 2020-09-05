package main

import (
	"os"
	"strings"
)

// Env is TK
type Env map[string]string

func environ() Env {
	env := make(Env)
	for _, e := range os.Environ() {
		fields := strings.SplitN(e, "=", 2)
		if len(fields) == 2 {
			env[fields[0]] = fields[1]
		}
	}

	return env
}
