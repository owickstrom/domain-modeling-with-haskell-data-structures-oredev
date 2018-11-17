TEX=xelatex
PLANTUML=deps/plantuml.jar

MASTER=src/master.tex
TEX_OPTIONS=options.tex
SRCS=$(shell find src -name '*.tex' -o -name '*.md' -o -name '*.css' -o -name '*.svg' -o -name '*.png' -o -name '*.uml' -o -name '*.dot' -o -name '*.hs') \
		 $(shell find src/examples -name '*.hs')

PANDOC_FLAGS= -s \
						  -f markdown+multiline_tables \
						  --filter pandoc-include-code \
						  --filter pandoc-emphasize-code \
						  -fmarkdown-implicit_figures

PANDOC_BEAMER_FLAGS=$(PANDOC_FLAGS) \
										-t beamer \
										--slide-level=2 \
										-H src/customizations.tex \
										-Vurlcolor=linkcolor \
										--listings

DIAGRAM_SRCS=$(shell find src/diagrams -name '*.dot')
DIAGRAMS=$(DIAGRAM_SRCS:src/diagrams/%.dot=target/diagrams/%.png)

UML_SRCS=$(shell find src/uml -name '*.uml.txt')
UMLS=$(UML_SRCS:src/uml/%.uml.txt=target/uml/%.png)

IMAGES_SRCS=$(shell find src/images -name '*.*')
IMAGES=$(IMAGES_SRCS:src/images/%=target/images/%)

SLIDES_DIR=target/slides
SLIDES=$(SLIDES_DIR)/slides.pdf

SLIDES_NO_NOTES_DIR=target/slides-no-notes
SLIDES_NO_NOTES=$(SLIDES_NO_NOTES_DIR)/slides-no-notes.pdf

.PHONY: all
all: html-slides programs

.PHONY: pdf-slides diagrams
pdf-slides: $(SLIDES) $(SLIDES_NO_NOTES)

.PHONY: html-slides diagrams
html-slides: target/html/index.html

target/slides.tex: $(SRCS)
	mkdir -p target
	pandoc $(PANDOC_BEAMER_FLAGS) \
		-H src/notes.tex \
		src/slides.md \
		-o $@

target/slides-no-notes.tex: $(SRCS)
	mkdir -p target
	pandoc $(PANDOC_BEAMER_FLAGS) -V classoption=handout src/slides.md -o $@

$(SLIDES): target/slides.tex $(IMAGES)
	rm -rf $(SLIDES_DIR)
	mkdir -p $(SLIDES_DIR)
	cp target/slides.tex $(SLIDES_DIR)/slides.tex
	cp -r src/images $(SLIDES_DIR)/images
	cd $(SLIDES_DIR) && \
		$(TEX) \
		-jobname slides \
		-halt-on-error \
		slides.tex

$(SLIDES_NO_NOTES): target/slides-no-notes.tex $(IMAGES)
	rm -rf $(SLIDES_NO_NOTES_DIR)
	mkdir -p $(SLIDES_NO_NOTES_DIR)
	cp target/slides-no-notes.tex $(SLIDES_NO_NOTES_DIR)/slides.tex
	cp -r src/images $(SLIDES_NO_NOTES_DIR)/images
	cd $(SLIDES_NO_NOTES_DIR) && \
		$(TEX) \
		-jobname slides-no-notes \
		-halt-on-error \
		slides.tex

target/html/index.html: $(SRCS) src/header.html src/theme.css $(IMAGES)
	mkdir -p target/html
	cp -r lib/* target/html/
	cp -r src/images target/html/
	cp -r target/uml target/html/uml
	cp -r target/diagrams target/html/diagrams
	cp src/theme.css target/html/reveal.js/css/theme/owickstrom.css
	pandoc $(PANDOC_FLAGS) \
		-t revealjs \
		-V theme=owickstrom \
		-V controls=true \
		-V transition=slide \
		-V transitionSpeed=fast \
		--no-highlight \
		-H src/header.html \
		src/slides.md \
		-o $@

target/diagrams/%.png: src/diagrams/%.dot
	mkdir -p target/diagrams
	dot -Tpng $< -o $@
	convert $@ -trim $@

target/uml/%.png: src/uml/%.uml.txt src/uml/styles.iuml $(PLANTUML)
	mkdir -p $(shell dirname $@)
	cat $< | java -jar $(PLANTUML) -tpng -pipe > $@
	convert $@ -trim $@

.PHONY: diagrams
diagrams: $(DIAGRAMS) $(UMLS)

programs:
	@echo "TODO: Compile programs."

target/images/%: src/images/%
	mkdir -p target/images
	cp $< $@

serve: html-slides
	(find src | entr -s 'make html-slides') &
	(cd target/html && python -m SimpleHTTPServer 10000)

.PHONY: pages
pages: html-slides
	rm -rf docs
	cp -r target/html/ docs

clean:
	rm -rf target

$(PLANTUML):
	mkdir -p $(shell dirname $@)
	wget http://sourceforge.net/projects/plantuml/files/plantuml.jar/download -O $@
