.PHONY: bootstrap test

test:
	cask exec buttercup -L tests -L site-lisp --eval "(setq undercover--send-report nil)" tests

bootstrap:
	cask exec emacs --batch -L site-lisp -l my-bootstrap -f my-create-cache

all: bootstrap test
