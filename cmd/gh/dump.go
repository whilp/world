package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"sort"
)

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

func dumpEnvironment(out io.Writer, env []string) {

	group := &Group{"environment", &bytes.Buffer{}}

	sort.Strings(env)
	for _, e := range env {
		if _, err := io.WriteString(group, fmt.Sprintln(e)); err != nil {
			return
		}
	}
	if err := group.Dump(out); err != nil {
		return
	}
}

func dumpPayload(out io.Writer, src []byte) {
	var buf bytes.Buffer
	if err := json.Indent(&buf, src, "", " "); err != nil {
		return
	}
	group := &Group{"payload", &buf}
	if err := group.Dump(out); err != nil {
		return
	}
}
