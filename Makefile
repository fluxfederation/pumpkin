default: js

ELM_FILES = $(shell find . -path elm/elm-stuff -prune -o -type f -name '*.elm')

js: ./public/js/app.js

public/js/app.js: $(ELM_FILES) elm/elm-stuff/.stamp
	cd elm && elm-make --yes src/Main.elm --output ../public/js/app.js

elm/elm-stuff/.stamp: elm/elm-package.json
	cd elm && elm-package install && touch ./elm-stuff/.stamp

clean-deps:
	rm -rf ./elm/elm-stuff

clean:
	rm -f public/js/app.js
	rm -rf elm/elm-stuff/build-artifacts

watch: js
	fswatch elm/src/ | while read; do make js; done

.DUMMY: js
