.PHONY: bootstrap test

test:
	cask exec buttercup -L tests \
                    -L site-lisp \
                    -L files \
                    --eval "(setq undercover--send-report nil)" \
                    ${BUTTERCUP_OPTIONS} tests

bootstrap:
	cask exec emacs --batch -L site-lisp -l my-bootstrap -f my-create-cache

all: bootstrap test
