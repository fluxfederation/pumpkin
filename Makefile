default: js

ELM_FILES = $(shell find elm/src -type f -name '*.elm')

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

realclean: clean clean-deps

watch: js
	@echo "Watching for changes..."
	@fswatch elm/src/ | while read; do clear; date; make js; done

.PHONY: js
.PHONY: realclean
