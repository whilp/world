package main

import (
	"context"
	"fmt"
)

func auth(env Env) error {
	ctx := context.Background()

	tr, err := getInstallationTransportFromEnv(ctx, env)
	if err != nil {
		return err
	}

	// client := github.NewClient(&http.Client{Transport: tr})
	accessToken, err := tr.Token(ctx)
	if err != nil {
		return err
	}

	fmt.Printf("::add-mask::%s\n", accessToken)
	fmt.Printf("::set-output name=token::%s\n", accessToken)

	return nil
}
