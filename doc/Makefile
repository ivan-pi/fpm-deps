

ASCIIDOCTOR = asciidoctor

.PHONY: all
all: fpm-tree.1 fpm-deps.1

%.1 : %.adoc
	$(ASCIIDOCTOR) -b manpage $<

fpm-tree.1: fpm-tree.adoc
fpm-deps.1: fpm-deps.adoc

.PHONY: clean
clean:
	$(RM) *.1
