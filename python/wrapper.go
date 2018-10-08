package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strings"
	"syscall"
)

func main() {
	args := os.Args
	env := os.Environ()
	if err := wrap(args, env); err != nil {
		log.Fatal(err)
	}
}

func wrap(args []string, env []string) error {
	python, _ := guess(args)

	args = append([]string{python}, args[1:]...)
	return syscall.Exec(python, args, env)
}

func guess(args []string) (string, error) {
	python := python2()

	if len(args) < 2 {
		return python, fmt.Errorf("missing file argument")
	}
	file, err := os.Open(os.Args[1])
	if err != nil {
		return python, err
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	scanner.Scan()
	shebang := scanner.Text()
	if err := scanner.Err(); err != nil {
		return python, err
	}

	switch {
	case strings.Contains(shebang, "python3"):
		python = python3()
	default:
		python = python2()
	}
	return python, nil
}

func python2() string {
	return "/usr/bin/python2"
}

func python3() string {
	return "/usr/bin/python3"
}
