## Variables
base = intro_ocaml
mlfile = $(base).ml
htmlfile = $(base).html
orgfile = $(base).org
exe = $(base).byt
bibfile = biblio.bib

.phony: help
# Use awk to parse the Makefile and generate a help menu.
# Add `## your description` after a `target:` to make it appear in the help menu.
help: ## Display this help section
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z0-9_-]+:.*?## / {printf "%-20s%s\n", $$1, $$2}' $(MAKEFILE_LIST)

default: ## Default is compile and html
default: compile $(htmlfile)

compile: $(exe)

extract: ## Extract (tangle) OCaml code from org file
extract: $(mlfile)
$(mlfile): $(orgfile)
	emacs --batch -l org $< -f org-babel-tangle

exe: ## Compile tangled OCaml from org file
exe: $(exe)
$(exe): $(mlfile) extract
	ocamlc -o $@ $<

.phony: clean
clean: ## Cleanup everything
	rm -f $(mlfile) $(exe) *.cmi *.cmo *.vrb *.pdfpc *.pdf *.log $(base).tex \
	$(htmlfile) *.pyg *~ *.bbl

html: ## Create html version from org file
html: $(htmlfile)
$(htmlfile): $(orgfile) $(bibfile)
	emacs $< --batch -l ./orghtml.el --kill
	sed -i -e "s/'Objective Caml'/'OCaml'/g" $@

.phony: launch
launch: ## setup X.org parameters and launch pdf presenter
	xset s off
	xset -dpms
	pdfpc -n right $(base).pdf
