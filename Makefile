.PHONY: bootstrap test travis travis-setup

# ORG MODE

tangled_files = $(patsubst %.org, %-tangled.el, $(wildcard files/*.org) $(wildcard site-lisp/*.org))

files/%-tangled.el: files/%.org
	cask exec emacs --batch --eval "(progn (require 'ob-tangle) (org-babel-tangle-file \"$<\"))"

site-lisp/%-tangled.el: site-lisp/%.org
	cask exec emacs --batch --eval "(progn (require 'ob-tangle) (org-babel-tangle-file \"$<\"))"

tangle: $(tangled_files)

# TESTING

test:
	cask exec buttercup -L tests \
                    -L site-lisp \
                    -L files \
                    --eval "(setq undercover--send-report nil)" \
                    ${BUTTERCUP_OPTIONS} tests

# TRAVIS

travis-before-install:
	cp Cask.travis Cask
	cask install
	git clone https://github.com/Fuco1/dired-hacks projects/dired-hacks
	git clone https://github.com/Fuco1/org-clock-budget projects/org-clock-budget
	git clone https://github.com/Fuco1/org-timeline projects/org-timeline
	git clone https://github.com/Fuco1/sallet projects/sallet

travis:
	$(MAKE) travis-before-install
	$(MAKE) tangle
	$(MAKE) test

# BOOTSTRAP

bootstrap:
	cask exec emacs --batch -L site-lisp -l my-bootstrap -f my-create-cache

all:
	$(MAKE) bootstrap
	$(MAKE) tangle
