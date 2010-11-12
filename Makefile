OBJDIR := build

ifeq ($(DO_PROF),1)
  OBJDIR = profbuild
endif

ifneq ($(OBJDIR),$(notdir $(CURDIR)))
include target.mk
else
#----- End Boilerplate

ifeq ($(MAKECMDGOALS),install)
ifndef PREFIX
$(error install evoked without definition of PREFIX)
endif
endif

VPATH=$(TOPDIR)/src:$(TOPDIR)/batteries:$(TOPDIR)

SRCDIR=$(TOPDIR)/src
DOCDIR=$(TOPDIR)/doc

# $(info VPATH = $(VPATH))

BATTERIES_SOURCES = enum.ml enum.mli dynArray.ml pMap.ml pMap.mli return.ml return.mli pSet.ml pSet.mli bitSet.ml bitSet.mli
YAKKER_SOURCES := logging.ml util.mli util.ml ykBuf.ml \
           wf_set.ml cs.ml pam_internal.mli pam_internal.ml pamJIT.mli pamJIT.ml \
	   yakker.mli yakker.ml history.mli history.ml \
           allp.ml pami.ml viz.ml engine.ml

SOURCES := $(BATTERIES_SOURCES) $(YAKKER_SOURCES)

MLI_SOURCES := $(filter %.mli, $(SOURCES))
ML_SOURCES := $(filter %.ml, $(SOURCES))

# Everything has a .cmi, whether or not it has a .mli file.
# However, we only care about modules with at least a .ml file,
# so we use that list of files to create the CMIS list.
CMIS:=$(ML_SOURCES:.ml=.cmi)
CMOS:=$(ML_SOURCES:.ml=.cmo)
CMXS:=$(ML_SOURCES:.ml=.cmx)

FRONT_END_SOURCES := compileopt.ml tgraph.ml gil.ml gul.ml desugar.ml pr.ml nullable_pred.ml minus.ml hash.ml copyrule.ml \
                     analyze.ml fusion.ml attributes.ml wrap.ml coroutine.ml replay.ml dispatch.ml label.ml lift.ml fsm.ml \
		     extract_grammar.ml rfc.ml yakker_grammar.ml cmdline.ml gil_gen.ml version.ml lexutil.ml main.ml
FRONT_END_ML_SOURCES := $(filter %.ml, $(FRONT_END_SOURCES))
FRONT_END_CMOS := $(FRONT_END_ML_SOURCES:.ml=.cmo)
FRONT_END_CMXS := $(FRONT_END_ML_SOURCES:.ml=.cmx)

OCAMLDOC_SOURCES := $(filter %.mli, $(YAKKER_SOURCES)) $(filter %.mli, $(FRONT_END_SOURCES))

M4PP = m4

#OCAMLOPT_FLAGS = -g
ifeq ($(DO_PROF),1)
  OCAMLC=ocamlfind ocamlcp
  OCAMLOPT_FLAGS += -p
else
  OCAMLC=ocamlfind ocamlc
endif
#  OCAMLC=~/Downloads/ocaml-3.12.0/boot/ocamlrun ~/Downloads/ocaml-3.12.0/boot/ocamlc  -I /Users/yitzhakm/Downloads/ocaml-3.12.0/boot
OCAMLOPT=ocamlfind ocamlopt
OCAMLDOC=ocamlfind ocamldoc

#ENGINE_TESTS = simple thunk
ENGINE_TESTS = thunk
ETESTS_EXE = $(foreach test,$(ENGINE_TESTS),$(test)-parser)
ETESTS_ML = $(foreach test,$(ENGINE_TESTS),$(test).ml)

# TJIM: removed "history" test below, temporary, conflict with new history.ml
#TESTS=history
TESTS = expr int255 intFW scott_example3 yxml2 imap t000 t001 t002 t003 t004 \
	t006 t007 \
        extract2 blackbox recur_w_args staract eof empty \
	starchar pexpr2
TESTS_EXE := $(foreach test,$(TESTS),$(test)-parser)
TESTS_OPT_EXE := $(foreach test,$(TESTS),$(test)-parser.opt)
TESTS_PCOMB_EXE := $(foreach test,$(TESTS),$(test)-pcomb-parser)
TESTS_PCOMB_OPT_EXE := $(foreach test,$(TESTS),$(test)-pcomb-parser.opt)
TESTS_ML := $(foreach test,$(TESTS),$(test).ml)
TESTS_PCOMB_ML := $(foreach test,$(TESTS),$(test)_pcomb.ml)

# Examples requiring the -inline-cs option.
EXAMPLES_ICS = ocaml_opt python ocamlyacc
EXAMPLES_ICS_ML := $(foreach example,$(EXAMPLES_ICS),$(example).ml)

EXAMPLES := aurochs aurochs_cor ocaml pexpr pyexpr js mailapp lgul $(EXAMPLES_ICS)
EXAMPLES_EXE := $(foreach example,$(EXAMPLES),$(example)-parser)
EXAMPLES_PCOMB_EXE := $(foreach example,$(EXAMPLES),$(example)-pcomb-parser)
EXAMPLES_OPT_EXE := $(foreach example,$(EXAMPLES),$(example)-parser.opt)
EXAMPLES_PCOMB_OPT_EXE := $(foreach example,$(EXAMPLES),$(example)-pcomb-parser.opt)
EXAMPLES_ML := $(foreach example,$(EXAMPLES),$(example).ml)
EXAMPLES_PCOMB_ML := $(foreach example,$(EXAMPLES),$(example)_pcomb.ml)

ARCHIVE_base = yakker
ARCHIVE = ${ARCHIVE_base}.cma
XARCHIVE = ${ARCHIVE_base}.cmxa
PACKAGE = yakker

# Program used to time the execution of generated parsers. Default is the standard time program
TIME_EXECUTION = time -p


######################################################################

all: yakker.cma yakker.cmxa yakker

.PHONY: install
install:
	@if test -f ${XARCHIVE}; then\
	  extralib="${XARCHIVE} ${ARCHIVE_base}.a";\
	fi;\
	echo "requires = \"unix\"" > META;\
	echo "description = \"The Yakker runtime library.\"" >> META;\
	echo "version = \"3.0\"" >> META;\
	echo "archive(byte) = \"${ARCHIVE}\"" >> META;\
	echo "archive(native) = \"${XARCHIVE}\"" >> META;\
	ocamlfind install ${PACKAGE} META ${CMIS} ${ARCHIVE} $$extralib
	godi_install yakker ${PREFIX}/bin

uninstall:
	ocamlfind remove yakker

.PHONY: tests
tests: $(ETESTS_EXE) $(TESTS_EXE)

.PHONY: examples
examples: $(EXAMPLES_EXE)

.PHONY: doc
doc: $(OCAMLDOC_SOURCES)
	$(OCAMLDOC) -html -d $(DOCDIR) -sort -t "Yakker Module Index" -short-functors $^

# GNU make hack. We turn it on to allow using a pattern-stem more than once
# in the prerequisite list. See, for example, the static pattern for the
# TESTS_ML targets.
.SECONDEXPANSION:

-include $(FRONT_END_ML_SOURCES:.ml=.d) $(ML_SOURCES:.ml=.d)
-include $(MLI_SOURCES:.mli=.di)

yakker.cma: $(CMOS)
	@echo "--x> " $@
	@$(OCAMLC) -g -a -o $@ -package unix $^

yakker.cmxa: $(CMXS)
	@echo "--x> " $@
	@$(OCAMLOPT) $(OCAMLOPT_FLAGS) -a -o $@  -package unix $^

yakkertest.cma: $(CMOS)
	@echo "--x> " $@
	@$(OCAMLC) -a  -o $@ -package unix $^

yakkertest.cmxa: $(CMXS)
	@echo "--x> " $@
	@$(OCAMLOPT) $(OCAMLOPT_FLAGS) -a -o $@ -package unix $^

universal-parser: yakker.cma universal_parser.cmo
	$(OCAMLC) $(PMLRUNTIME) $^ -o $@

stringify: stringify.cmo
	@echo "--x> " $@
	@$(OCAMLC) $^ -o $@

strings.ml: core.bnf stringify
	@echo "x--> " $@
	@./stringify $< > $@

rfcs.ml: $(SRCDIR)/rfcs stringify
	@echo "x--> " $@
	@./stringify -rfcs $< > $@

buildinfo.ml:
	@echo "x--> " $@
	@echo "let build_dir = \"$(CURDIR)\"" > $@

DEPEND.pdf: $(SOURCES) $(FRONT_END_SOURCES)
	ocamldep -I $(TOPDIR)/src -I $(TOPDIR)/batteries $^ | ocamldot | dot -Tpdf > $@

# Qian's debugging stuff
yakker-pcomb-parser.opt: tgraph.cmx bnf.cmx yakker.cmxa
	@echo "x--> yakker_pcomb.ml"
	@./yakker compile -backend fun ../examples/lex/yakker_grammar.bnf > yakker_pcomb.ml
	@echo "-x-> yakker_pcomb.cmx"
	@$(OCAMLOPT) -c yakker_pcomb.ml -o yakker_pcomb.cmx
	@echo "--x> yakker-pcomb-parser.opt"
	@$(OCAMLOPT) yakker.cmxa tgraph.cmx bnf.cmx yakker_pcomb.cmx -package unix -linkpkg -o yakker-pcomb-parser.opt

yakker-lex-pcomb-parser.opt: tgraph.cmx bnf.cmx yakker.cmxa
	@echo "x--> lex_pcomb.ml"
	@./yakker compile -backend fun ../examples/lex/lex_debug.bnf > lex_pcomb.ml
	@echo "-x-> lex_pcomb.cmx"
	@$(OCAMLOPT) -c lex_pcomb.ml -o lex_pcomb.cmx
	@echo "--x> lex-pcomb-parser.opt"
	@$(OCAMLOPT) yakker.cmxa tgraph.cmx bnf.cmx lex_pcomb.cmx -package unix -linkpkg -o lex-pcomb-parser.opt

######################### REGRESSION TESTING #########################
# Quick guide:
#  To run:
#      make regress
#  To update regressions:
#      make update-regress
#  To show the output of all regression tests:
#      make show-regress
#  To add regression tests in the tests or examples directory,
# in the test or example directory add directories input and output,
# create one file per input, update Makefile variable R_TESTS or
# R_EXAMPLES, then
#      make update-regress
#  Don't forget to bzr add your new directories/files.

# TODO: imap aurochs ocaml
# TJIM: removed "history" test below, temporary, conflict with new history.ml
#R_TESTS = expr int255 intFW scott_example3 yxml2 extract2 t000 t001 t002 t003 t004 recur_w_args staract blackbox empty eof
# YHM: removed "extract2" test below, because it has been broken for a while.
R_TESTS = expr int255 intFW scott_example3 yxml2 t000 t001 t002 t003 t004 blackbox empty eof t006 t007
R_EXAMPLES = pexpr pyexpr mailapp python

C_T := $(foreach i,$(R_TESTS),$(i)-parser.opt)
C_E := $(foreach i,$(R_EXAMPLES),$(i)-parser.opt)
R_T := $(foreach i,$(R_TESTS),run-t-$(i))
R_E := $(foreach i,$(R_EXAMPLES),run-e-$(i))
U_T := $(foreach i,$(R_TESTS),update-t-$(i))
U_E := $(foreach i,$(R_EXAMPLES),update-e-$(i))
S_T := $(foreach i,$(R_TESTS),show-t-$(i))
S_E := $(foreach i,$(R_EXAMPLES),show-e-$(i))
P_T := $(foreach i,$(R_TESTS),perf-t-$(i))
P_E := $(foreach i,$(R_EXAMPLES),perf-e-$(i)) perf-e-ocaml
RP_T := $(foreach i,$(R_TESTS),recperf-t-$(i))
RP_E := $(foreach i,$(R_EXAMPLES),recperf-e-$(i))
PC_T := $(foreach i,$(R_TESTS),pc-perf-t-$(i))
PC_E := $(foreach i,$(R_EXAMPLES),pc-perf-e-$(i)) pc-perf-e-ocaml

.PHONY: regress update-regress show-regress compile-regress perf-test record-perf-test

regress: $(R_T) $(R_E)
compile-regress: $(C_T) $(C_E)
update-regress: $(U_T) $(U_E)
show-regress: $(S_T) $(S_E)
perf-test: $(P_T) $(P_E)
record-perf-test: $(RP_T) $(RP_E)

.PHONY: $(R_T) $(R_E) $(U_T) $(U_R) $(S_T) $(S_R) $(P_T) $(P_E) $(PC_T) $(PC_E) $(RP_T) $(RP_E)
run-t-% update-t-% show-t-% perf-t-% pc-perf-t-% recperf-t-%: D=../tests
run-e-% update-e-% show-e-% perf-e-% pc-perf-e-% recperf-e-%: D=../examples

define run-regression
	@echo testing $*
	@for i in $(D)/$*/inputs/*;\
	  do ./$*-parser.opt -new-engine $$i 2>&1 |\
		cmp -s - $(D)/$*/outputs/`basename $$i` || echo '    FAILURE:' `basename $$i`;\
	done
endef

define update-regression
	@echo updating regression tests for $*
	@mkdir -p $(D)/$*/outputs
	@for i in $(D)/$*/inputs/*;\
	  do ./$*-parser.opt -new-engine $$i 2>&1 |\
		cmp -s - $(D)/$*/outputs/`basename $$i` ||\
		echo '    updating' $* on `basename $$i` &&\
		./$*-parser.opt $$i > $(D)/$*/outputs/`basename $$i` 2>&1 ;\
	done
endef

define show-regression
	@echo '***********************************' $* '***********************************'
	@for i in $(D)/$*/inputs/*;\
	  do ./$*-parser.opt -new-engine $$i ;\
	done
endef

# args: output -- output file name
define perf-test
 for i in $(D)/$*/inputs/*; do\
   echo $*'|'`basename $$i`'|'tx >> $$output ;\
   ($(TIME_EXECUTION) ./$*-parser.opt $$i) 2>> $$output ;\
   echo $*'|'`basename $$i`'|'fun >> $$output ;\
   ($(TIME_EXECUTION) ./$*-pcomb-parser.opt $$i) 2>> $$output ;\
 done
endef

# performance test parser combinators only.
# args: output -- output file name
define perf-test-pc
 for i in $(D)/$*/inputs/*; do\
   echo $*'|'`basename $$i`'|'fun >> $$output ;\
   ($(TIME_EXECUTION) ./$*-pcomb-parser.opt $$i) 2>> $$output ;\
 done
endef

# args: none
define show-perf-test
echo '*******************************' Timing $* '************************************' ;\
 output=/dev/stdout ;\
 $(perf-test)
endef

# performance test parser combinators only.
# args: none
define show-perf-test-pc
echo '*******************************' Timing $* '************************************' ;\
 output=/dev/stdout ;\
 $(perf-test-pc)
endef

# args: none
define record-perf-test
mkdir -p $(D)/$*/perf-archive; \
 testdate=`date +"%F-%H-%M-%S"` ;\
 output=$(D)/$*/perf-archive/test-$$testdate ;\
 echo $$testdate'|'`hostname -s` >> $$output ; \
 $(perf-test)
endef

$(R_T): run-t-% : %-parser.opt
	$(run-regression)

$(R_E): run-e-% : %-parser.opt
	$(run-regression)

$(U_T): update-t-% : %-parser.opt
	$(update-regression)

$(U_E): update-e-% : %-parser.opt
	$(update-regression)

$(S_T): show-t-% : %-parser.opt
	$(show-regression)

$(S_E): show-e-% : %-parser.opt
	$(show-regression)

$(P_T): perf-t-% : %-parser.opt %-pcomb-parser.opt
	@($(show-perf-test))

$(P_E): perf-e-% : %-parser.opt %-pcomb-parser.opt
	@($(show-perf-test))

$(PC_T): pc-perf-t-% : %-pcomb-parser.opt
	@($(show-perf-test-pc))

$(PC_E): pc-perf-e-% : %-pcomb-parser.opt
	@($(show-perf-test-pc))

$(RP_T): recperf-t-% : %-parser.opt %-pcomb-parser.opt
	@($(record-perf-test))

$(RP_E): recperf-e-% : %-parser.opt %-pcomb-parser.opt
	@($(record-perf-test))

############################## PATTERNS ##############################

%.d: %.ml
	@echo "---> " $@
	@(cd $(dir $<); ocamldep $(notdir $<)) >$@

%.di: %.mli
	@echo "---> " $@
	@(cd $(dir $<); ocamldep $(notdir $<)) >$@

%.cmi: %.mli
	@echo "-x-> " $@
	@$(OCAMLC) $(OCAML_FLAGS) -c $< -o $@

%.cmo %.cmi: %.ml
	@echo "-x-> " $@
	@$(OCAMLC) $(OCAML_FLAGS) -g -c $< -o $@

%.cmx: %.ml
	@echo "-x-> " $@
	@$(OCAMLOPT) $(OCAMLOPT_FLAGS) -c $< -o $@

%.dot: %.bnf yakker
	@echo "--x> " $@
	@./yakker dot $< > $@

%.ml: %.bnf yakker
	@echo "x--> " $@
	@./yakker compile $< > $@

.PHONY: show-%.mli
show-%.mli : %.ml
	@$(OCAMLC) $(OCAML_FLAGS) -i $<

########################################################################

########################################
## Bootstrap-related targets
########################################

.PHONY: update-yakker restore-yakker
.PHONY: bootstrap-yakker_grammar bootstrap-cmdline bootstrap-extract_grammar
.PHONY: gen-engine
.PHONY: restore-yakker_grammar restore-cmdline restore-extract_grammar restore-engine

bootstrap-yakker_grammar bootstrap-cmdline bootstrap-extract_grammar: bootstrap-%: $(SRCDIR)/syntax/%.bnf
	@echo checking bootstrapped file $*.ml
	@./yakker compile $(SRCDIR)/syntax/$*.bnf |\
		cmp -s - $(SRCDIR)/$*.ml ||\
		(echo '    updating' $* && cp $(SRCDIR)/$*.ml $(SRCDIR)/prev_$*.ml && ./yakker compile $(SRCDIR)/syntax/$*.bnf > $(SRCDIR)/$*.ml)

gen-engine: gen-% : $(SRCDIR)/wfe.ml
	@echo checking generated file $*.ml
	@$(M4PP) $< | cmp -s - $(SRCDIR)/$*.ml ||\
		(echo '    updating' $* && cp $(SRCDIR)/$*.ml $(SRCDIR)/prev_$*.ml && $(M4PP) $< > $(SRCDIR)/$*.ml)

update-yakker: bootstrap-yakker_grammar bootstrap-cmdline bootstrap-extract_grammar gen-engine

restore-yakker_grammar restore-cmdline restore-extract_grammar restore-engine: restore-%:
	mv $(SRCDIR)/prev_$*.ml $(SRCDIR)/$*.ml

restore-yakker: restore-yakker_grammar restore-cmdline restore-extract_grammar restore-engine

########################################

ifndef NO_YR
yakker-byte: yakker.cma strings.cmo rfcs.cmo buildinfo.cmo $(FRONT_END_CMOS)
	@echo "--x> " $@
	@$(OCAMLC) $^ -g -package unix -linkpkg -o $@

yakker: yakker.cmxa strings.cmx rfcs.cmx buildinfo.cmx $(FRONT_END_CMXS)
	@echo "--x> " $@
	@$(OCAMLOPT) $^ -package unix -linkpkg -o $@
endif

########################################################################
##   Tests
########################################################################

$(ETESTS_EXE) : %-parser : yakker.cma %.cmo
	@echo "--x> " $@
	@$(OCAMLC) $^ -g -package unix -linkpkg -o $@


$(ETESTS_ML) : %.ml : tests/$$*/$$*.bnf yakker
	@echo "x--> " $@
	@./yakker compile-only $< > $@

########################################################################

$(TESTS_OPT_EXE): %-parser.opt: yakker.cmxa %.cmx
	@echo "--x> " $@
	@$(OCAMLOPT) $^ -package unix -linkpkg -o $@

$(TESTS_PCOMB_OPT_EXE): %-pcomb-parser.opt: yakker.cmxa %_pcomb.cmx
	@echo "--x> " $@
	@$(OCAMLOPT) $^ -package unix -linkpkg -o $@

$(TESTS_EXE): %-parser: yakker.cma %.cmo
	@echo "--x> " $@
	@$(OCAMLC) $^ -g -package unix -linkpkg -o $@

$(TESTS_PCOMB_EXE): %-pcomb-parser: yakker.cma %_pcomb.cmo
	@echo "--x> " $@
	@$(OCAMLC) $^ -g -package unix -linkpkg -o $@

$(TESTS_ML): %.ml : tests/$$*/$$*.bnf yakker
	@echo "x--> " $@
	@./yakker compile $(YOPTS) $< > $@

$(TESTS_PCOMB_ML): %_pcomb.ml : tests/$$*/$$*.bnf yakker
	@echo "x--> " $@
	@./yakker compile -backend fun $< > $@

########################################################################
##   Standardized examples
########################################################################

$(EXAMPLES_OPT_EXE): %-parser.opt: yakker.cmxa %.cmx
	@echo "--x> " $@
	@$(OCAMLOPT) $^ -package unix -linkpkg -o $@

$(EXAMPLES_PCOMB_OPT_EXE): %-pcomb-parser.opt: yakker.cmxa %_pcomb.cmx
	@echo "--x> " $@
	@$(OCAMLOPT) $^ -package unix -linkpkg -o $@

$(EXAMPLES_EXE): %-parser: yakker.cma %.cmo
	@echo "--x> " $@
	@$(OCAMLC) $^ -g -package unix -linkpkg -o $@

$(EXAMPLES_PCOMB_EXE): %-pcomb-parser: yakker.cma %_pcomb.cmo
	@echo "--x> " $@
	@$(OCAMLC) $^ -g -package unix -linkpkg -o $@

$(EXAMPLES_ICS_ML): YOPTS+=-inline-cs

$(EXAMPLES_ML): %.ml : examples/$$*/$$*.bnf yakker
	@echo "x--> " $@
	@./yakker compile $(YOPTS) $< > $@

$(EXAMPLES_PCOMB_ML): %_pcomb.ml : examples/$$*/$$*.bnf yakker
	@echo "x--> " $@
	@./yakker compile -backend fun $< > $@

########################################################################
##  Specialized tests
########################################################################

OCAML_COMP_DIR=/Users/yitzhakm/sw/godi/lib/ocaml/compiler-lib
OCAML_COMP_INCLUDES = -I $(OCAML_COMP_DIR)

ocamlparser: yakker.cma ocamlparser.cmo llexer.cmo opdriver.cmo
	$(OCAMLC) $(OCAML_COMP_DIR)/config.cmo \
           $(OCAML_COMP_DIR)/misc.cmo \
           $(OCAML_COMP_DIR)/clflags.cmo $(OCAML_COMP_DIR)/linenum.cmo \
           $(OCAML_COMP_DIR)/warnings.cmo \
           $(OCAML_COMP_DIR)/location.cmo \
           $(OCAML_COMP_DIR)/syntaxerr.cmo \
           $^ -package unix -linkpkg -o $@

ocamlparser.cmx llexer.cmx opdriver.cmx : %.cmx: %.ml
	$(OCAMLOPT) $(OCAML_COMP_INCLUDES) -c $< -o $@

ocamlparser.cmo llexer.cmo opdriver.cmo : %.cmo: %.ml
	$(OCAMLC) $(OCAML_COMP_INCLUDES) -c $< -o $@

ocamlparser.cmi: %.cmi : %.mli
	$(OCAMLOPT) $(OCAML_COMP_INCLUDES) -c $< -o $@

opdriver.cmo: ocamlparser.cmi llexer.cmi
opdriver.cmx: ocamlparser.cmi llexer.cmi
ocamlparser.cmo: ocamlparser.cmi
ocamlparser.cmx: ocamlparser.cmi
llexer.cmo: ocamlparser.cmi
llexer.cmx: ocamlparser.cmi

#ocamlparser.ml:
#	yakkeropt compile ../tmp/parser4.bnf > ocamlparser.ml

llexer.ml: llexer.mll
	ocamllex $<

###############################

ocamlp-w-c: yakker.cmxa ocamlpwc.cmx
	$(OCAMLOPT) $^ -package unix -linkpkg -o $@

ocamlpwc.ml: examples/ocaml/ocaml-with-coroutines.bnf yakker
	@echo "x--> " $@
	@./yakker compile $< > $@

ocamlp-wo-c: yakker.cmxa ocamlpwoc.cmx
	$(OCAMLOPT) $^ -package unix -linkpkg -o $@

ocamlpwoc.ml: examples/ocaml/ocaml-without-coroutines.bnf yakker
	@echo "x--> " $@
	@./yakker compile $< > $@


aurochswoc-parser aurochswc-parser: %-parser: yakker.cmxa %.cmx
	$(OCAMLOPT) $^ -package unix -linkpkg -o $@

aurochswoc.ml aurochswc.ml: %.ml : %.bnf yakker
	@echo "x--> " $@
	@./yakker compile $< > $@

aurochswc.bnf : examples/aurochs_cor/aurochs_cor.bnf
	@echo "Transforming " $<
	@./yakker transform strip-late,unroll $< > $@

aurochswoc.bnf : examples/aurochs/aurochs.bnf
	@echo "Transforming " $<
	@./yakker transform strip-late,unroll $< > $@
	@echo '@end { Pami.Simple.qtest parse_file visualize_file }' >> $@



DYPHOME = ~/sw/dypgen
dyptest.cmo: dyptest.ml
	@echo "-x-> " $@
	@$(OCAMLC) $(OCAML_FLAGS) -I $(DYPHOME)/dyplib -g -c $< -o $@

dyptest.cmx: dyptest.ml
	@echo "-x-> " $@
	@$(OCAMLOPT) $(OCAML_FLAGS) -I $(DYPHOME)/dyplib -c $< -o $@

#wfe : yakker.cmxa ocaml_woc.cmx python2.cmx engine.ml test_wfe.ml
wfe : yakker.cmxa ocaml_woc.cmx
	$(OCAMLOPT) -package unix -linkpkg -inline 30 $^ -o $@

wfe_g : yakker.cma ocaml_woc.cmo python2.cmo engine.ml test_wfe.ml
	$(OCAMLC) -g -package unix -linkpkg $^ -o $@


########################################################################
##  yakker component tests
########################################################################

test_ykBuf test_cs : test_% : yakker.cmxa test_%.cmx
	$(OCAMLOPT) -package unix -linkpkg $^ -o $@

########################################################################

aurochs-tx: yakker.cma tgraph.cmo bnf.cmo cs.cmo pr.cmo aurochs.cmo aurochs_tx.cmo
	@echo "--x> " $@
	@$(OCAMLC) $^ -g -package unix -linkpkg -o $@

aurochs_tx.cmo : examples/aurochs/aurochs_tx.ml aurochs.cmo
	@echo "-x-> " $@
	@$(OCAMLC) $(OCAML_FLAGS) -g -c $< -o $@

.PHONY: aurochs-depend
aurochs-depend: aurochs.ml aurochs_tx.ml
	ocamldep $^ > examples/aurochs/.depend

-include examples/aurochs/.depend

.PHONY: all opt examples

#----- Begin Boilerplate
endif
