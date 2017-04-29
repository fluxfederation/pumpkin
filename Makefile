default: js

ELM_FILES = $(shell find elm/src -type f -name '*.elm' -not -name '.\#*')

js: public/js/app.js

public/js/app.js: $(ELM_FILES) elm/elm-stuff/.stamp
	cd elm && elm-make --yes src/Main.elm --output ../public/js/app.js

elm/elm-stuff/.stamp: elm/elm-package.json
	cd elm && elm-package install --yes && touch ./elm-stuff/.stamp

clean-deps:
	rm -rf ./elm/elm-stuff

clean:
	rm -f public/js/app.js
	rm -rf elm/elm-stuff/build-artifacts

debug:
	cd elm && elm-make --debug --yes src/Main.elm --output ../public/js/app.js

realclean: clean clean-deps

watch:
	@make js || true
	@echo "Watching for changes..."
	@fswatch elm/src/ | grep -v '.\#' | while read changed; do date; echo "MODIFIED: $$changed"; make js || true; done

.PHONY: js
.PHONY: realclean
