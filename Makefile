default: app.js

ELM_FILES = $(shell find . -path ./elm/elm-stuff -prune -o -type f -name '*.elm')

app.js: $(ELM_FILES)
	cd ./elm && elm-make --yes ./src/Main.elm --output ../public/js/app.js

clean-deps:
	rm -rf ./elm/elm-stuff

clean:
	rm -f public/js/app.js
	rm -rf ./elm/elm-stuff/build-artifacts
