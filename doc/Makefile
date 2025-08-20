

ASCIIDOCTOR = asciidoctor
HTMLDIR = ./html

.PHONY: all
all: fpm-tree.1 fpm-deps.1

%.1 : %.adoc
	$(ASCIIDOCTOR) -b manpage $<

fpm-tree.1: fpm-tree.adoc
fpm-deps.1: fpm-deps.adoc


.PHONY: html
html: $(HTMLDIR)/index.html \
	  $(HTMLDIR)/fpm-tree.html \
	  $(HTMLDIR)/fpm-deps.html

$(HTMLDIR)/%.html: %.adoc
	$(ASCIIDOCTOR) -o $@ $<



.PHONY: $(HTMLDIR)/depgraph.html
	
$(HTMLDIR)/depgraph.html:
	fpm run --target fpm-deps -- --mermaid html -o ./doc/$@


.PHONY: clean
clean:
	$(RM) *.1 *.html
