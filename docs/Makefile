

ASCIIDOCTOR = asciidoctor
HTMLDIR = ./html

DOCDIR  = ./
ADOCDIR = $(DOCDIR)/adoc


.PHONY: all
all: html depgraph


.PHONY: man
man: fpm-tree.1 fpm-deps.1

%.1 : $(ADOCDIR)/%.adoc
	$(ASCIIDOCTOR) -b manpage $<


.PHONY: html
html: $(DOCDIR)/index.html \
	  $(DOCDIR)/fpm-tree.html \
	  $(DOCDIR)/fpm-deps.html

$(DOCDIR)/%.html: $(ADOCDIR)/%.adoc
	$(ASCIIDOCTOR) -o $@ $<


.PHONY: depgraph
depgraph: $(DOCDIR)/depgraph.html

$(DOCDIR)/depgraph.html:
	fpm run --target fpm-deps -- --mermaid html -o ./doc/$@

.PHONY: clean
clean:
	$(RM) *.1
	$(RM) $(DOCDIR)/*.html