package main

import (
	"strings"
)

// Env is TK
type Env map[string]string

func NewEnv(raw []string) Env {
	env := make(Env)
	n := 2
	for _, e := range raw {
		fields := strings.SplitN(e, "=", n)
		if len(fields) == n {
			env[fields[0]] = fields[1]
		}
	}

	return env
}

func (e Env) Get(key string, otherwise ...string) string {
	value, ok := e[key]
	if !ok {
		if len(otherwise) > 0 {
			value = otherwise[0]
		}
	}
	return value
}
