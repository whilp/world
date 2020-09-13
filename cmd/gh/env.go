package main

import (
	"os"
	"strings"
)

// Env is TK
type Env map[string]string

func environ() Env {
	env := make(Env)
	n := 2
	for _, e := range os.Environ() {
		fields := strings.SplitN(e, "=", n)
		if len(fields) == n {
			env[fields[0]] = fields[1]
		}
	}

	return env
}
