.PHONY: live app clean
all: app
live:
	npx elm-live src/Main.elm --open
app:
	mkdir -p site
	elm-make --yes src/Main.elm --output site/index.html
	cp -R data site
clean:
	rm -rf site
