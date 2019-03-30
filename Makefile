NAME=whokey
VERSION=$(shell cat version)

default: all

all:
	dune build @install
	@test -L bin || ln -s _build/install/default/bin .

clean:
	dune clean

distclean:
	@mkdir -p dist
	@rm -rf dist/*
	@docker rmi ${NAME} 2>/dev/null || true

build:
	@docker build -t ${NAME} --network=host .

dist: distclean build
	@docker run --name ${NAME}-dist --rm --detach --network=host -it ${NAME}
	@docker cp ${NAME}-dist:/${NAME}/_build/default/src/${NAME}.exe ./dist/${NAME}-${VERSION}
	@docker kill ${NAME}-dist

sign: dist
	$(eval key := $(shell git config --get user.signingkey))
	for file in dist/*; do \
		gpg2 --armor --local-user ${key} --detach-sign $${file}; \
	done

package: sign
	@tar czf dist/${NAME}-${VERSION}.tar.gz -C dist ${NAME}-${VERSION} ${NAME}-${VERSION}.asc
	@find dist/ -type f  ! -name "*.tar.gz" -delete

tag:
	script/tag.sh

upload:
	if [ ! -z "$${GITHUB_TOKEN}" ]; then \
		ghr -t "$${GITHUB_TOKEN}" -u $$(whoami) -r ${NAME} -replace ${VERSION} dist/; \
	fi

release: package tag upload

.PHONY: default all clean disclean build dist sign package tag upload release
