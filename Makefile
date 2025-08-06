.PHONY: all

all:
	nix run . build
	if [ -e public ]; then rm -rf public; fi
	cp -R _site public
