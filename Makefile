pwd := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

.PHONY: test

test:
	ros run -e "(asdf:load-asd \"$(pwd)/clish.asd\") (ql:quickload :clish) (asdf:test-system :clish) (uiop:quit 0)"
