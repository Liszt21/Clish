pwd := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

.PHONY: test clean

clean:
	rm -rf cache

test: clean
	ros manage.lisp test
