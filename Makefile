FPC_OPTIONS := $(shell $(CASTLE_ENGINE_PATH)castle_game_engine/scripts/castle_engine_fpc_options)
FPC_DEBUG := fpc -dDEBUG $(FPC_OPTIONS)
FPC_RELEASE := fpc -dRELEASE $(FPC_OPTIONS)

default: build-debug

.PHONY: build-debug
build-debug:
	$(FPC_DEBUG) grammar_compression.lpr
	$(FPC_DEBUG) mk_test_file.lpr

.PHONY: build-release
build-release:
	$(FPC_RELEASE) grammar_compression.lpr
	$(FPC_RELEASE) mk_test_file.lpr

REPORT.pdf: REPORT.tex
	pdflatex --file-line-error-style -interaction=nonstopmode REPORT.tex
	pdflatex --file-line-error-style -interaction=nonstopmode REPORT.tex

REPORT.dvi: REPORT.tex
	latex --file-line-error-style -interaction=nonstopmode REPORT.tex
	latex --file-line-error-style -interaction=nonstopmode REPORT.tex

.PHONY: pasdoc
pasdoc:
	mkdir -p apidoc/
	pasdoc *.pas --auto-abstract --auto-link \
	  --title="grammar_compression units" \
	  --output=apidoc/

# Clean things not intended to be inside dist, and at the same time
# things that can be remade.
.PHONY: clean-common
clean-common:
	rm -f *.ppu *.o *~ grammar_compression.tar.gz \
	  REPORT.aux REPORT.log REPORT.out REPORT.dvi

# Clean things that can be remade.
.PHONY: clean
clean: clean-common
	rm -f grammar_compression mk_test_file REPORT.pdf
	rm -Rf apidoc/

.PHONY: dist
dist:
	$(MAKE) clean
	$(MAKE) pasdoc build-release REPORT.pdf
	$(MAKE) clean-common
	rm -Rf /tmp/grammar_compression/
	mkdir /tmp/grammar_compression/
	cp -R . /tmp/grammar_compression/
	find /tmp/grammar_compression/ \
	  -type d '(' -name '.svn' -or -iname 'private' ')' \
	  -prune -exec rm -Rf '{}' ';'
	cd /tmp/; tar czvf grammar_compression.tar.gz grammar_compression/
	mv /tmp/grammar_compression.tar.gz .