#!/usr/bin/env python3

import sys

# Ensure we import the bandit requirement, not this wrapper module.
top = sys.path.pop(0)
sys.path.append(top)

import bandit.cli.main

if __name__ == "__main__":
    bandit.cli.main.main()
