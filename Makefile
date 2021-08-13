.PHONY: test

test:
	ros run -e "(asdf:load-asd (probe-file \"./clish.asd\")) (ql:quickload :clish) (asdf:test-system :clish) (uiop:quit 0)"
