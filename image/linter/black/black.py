#!/usr/bin/env python3

import sys

# Ensure we import the black requirement, not this wrapper module.
top = sys.path.pop(0)
sys.path.append(top)

import black

black.patched_main()
