
# Set the build directory:
ifdef DO_PROF
  OBJDIR = profbuild
else ifdef DO_LOG
  OBJDIR = logbuild
else
 OBJDIR = build
endif

ifndef DO_LOG
engine.cmo: OCAML_FLAGS+= -noassert
engine.cmx: OCAMLOPT_FLAGS+= -noassert
engine_hh.cmo: OCAML_FLAGS+= -noassert
engine_hh.cmx: OCAMLOPT_FLAGS+= -noassert
engine_nr.cmo: OCAML_FLAGS+= -noassert
engine_nr.cmx: OCAMLOPT_FLAGS+= -noassert
engine_fl.cmo: OCAML_FLAGS+= -noassert
engine_fl.cmx: OCAMLOPT_FLAGS+= -noassert
engine_nrfl.cmo: OCAML_FLAGS+= -noassert
engine_nrfl.cmx: OCAMLOPT_FLAGS+= -noassert
endif

ifneq ($(OBJDIR),$(notdir $(CURDIR)))
include target.mk
else
#----- End Boilerplate

ifdef USE_CORS
  $(info **Using coroutines**)
else
  $(info **Using arrow notation (default)**)
  YOPTS+=-arrow-notation
endif

#YOPTS+=-no-nullability-preds

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
YAKKER_SOURCES := logging.ml util.ml ykBuf.ml cs.ml \
           wf_set.ml pam_internal.mli pam_internal.ml pamJIT.mli pamJIT.ml \
	   history.mli history.ml viz.ml\
           allp.ml pami.ml engine.ml engine_hh.ml engine_nr.ml engine_fl.ml engine_nrfl.ml

SOURCES := $(BATTERIES_SOURCES) $(YAKKER_SOURCES)

MLI_SOURCES := $(filter %.mli, $(SOURCES))
ML_SOURCES := $(filter %.ml, $(SOURCES))

# Everything has a .cmi, whether or not it has a .mli file.
# However, we only care about modules with at least a .ml file,
# so we use that list of files to create the CMIS list.
CMIS:=$(ML_SOURCES:.ml=.cmi)
CMOS:=$(ML_SOURCES:.ml=.cmo)
CMXS:=$(ML_SOURCES:.ml=.cmx)

FRONT_END_SOURCES := compileopt.ml variables.ml tgraph.ml meta_prog.ml tyspec.ml gil.ml gul.ml pr.ml nullable_pred.ml \
                     minus.ml desugar.ml hash.ml copyrule.ml \
                     analyze.ml unroll.ml fusion.ml attributes.ml wrap.ml \
                     coroutine.ml replay.ml \
                     ty_infer.ml lift.ml inline_nullable.ml fsm.ml dearrow.ml \
		     extract_grammar.ml rfc.ml ocaml_lexer.mll ocamllex_lexer.mll yakker_grammar.ml \
                     cmdline.ml gil_gen.ml version.ml lexutil.ml lookahead.ml main.ml
FRONT_END_MLL_SOURCES := $(filter %.mll, $(FRONT_END_SOURCES))
FRONT_END_ML_SOURCES := $(FRONT_END_MLL_SOURCES:.mll=.ml) $(filter %.ml, $(FRONT_END_SOURCES))
FRONT_END_CMOS := $(FRONT_END_ML_SOURCES:.ml=.cmo)
FRONT_END_CMXS := $(FRONT_END_ML_SOURCES:.ml=.cmx)

OCAMLDOC_SOURCES := $(filter %.mli, $(YAKKER_SOURCES)) $(filter %.mli, $(FRONT_END_SOURCES))

M4PP = m4

# The engine uses a recursive type definition that doesn't go through
# a datatype, so we need to allow arbitrary recursive types.
OCAMLOPT_FLAGS += -rectypes
OCAML_FLAGS+= -rectypes

ifeq ($(DO_PROF),1)
  OCAMLOPT_FLAGS += -p
# Don't use ocamlcp because the .cmi files it produces are not compatible with ocamlopt
  OCAMLC=ocamlfind ocamlc
else
  OCAMLC=ocamlfind ocamlc
endif
#  OCAMLC=~/Downloads/ocaml-3.12.0/boot/ocamlrun ~/Downloads/ocaml-3.12.0/boot/ocamlc  -I /Users/yitzhakm/Downloads/ocaml-3.12.0/boot
OCAMLOPT=ocamlfind ocamlopt
OCAMLDOC=ocamlfind ocamldoc

# TJIM: removed "history" test below, temporary, conflict with new history.ml
#TESTS=history
TESTS = expr int255 intFW scott_example3 yxml2 imap t000 t001 t002 t003 t004 \
	t006 t007 \
        extract2 blackbox recur_w_args staract eof empty \
	starchar oldtyspec
TESTS_EXE := $(foreach test,$(TESTS),$(test)-parser)
TESTS_OPT_EXE := $(foreach test,$(TESTS),$(test)-parser.opt)
TESTS_PCOMB_EXE := $(foreach test,$(TESTS),$(test)-pcomb-parser)
TESTS_PCOMB_OPT_EXE := $(foreach test,$(TESTS),$(test)-pcomb-parser.opt)
TESTS_ML := $(foreach test,$(TESTS),$(test).ml)
TESTS_PCOMB_ML := $(foreach test,$(TESTS),$(test)_pcomb.ml)

# Examples requiring the -inline-cs option.
EXAMPLES_ICS = js ocaml_opt python ocamlyacc ocamlparser_regular cisco_ios
EXAMPLES_ICS_ML := $(foreach example,$(EXAMPLES_ICS),$(example).ml)

EXAMPLES := aurochs aurochs_cor ocaml pexpr pyexpr mailapp lgul happy $(EXAMPLES_ICS)
EXAMPLES_EXE := $(foreach example,$(EXAMPLES),$(example)-parser)
EXAMPLES_PCOMB_EXE := $(foreach example,$(EXAMPLES),$(example)-pcomb-parser)
EXAMPLES_OPT_EXE := $(foreach example,$(EXAMPLES),$(example)-parser.opt)
EXAMPLES_PCOMB_OPT_EXE := $(foreach example,$(EXAMPLES),$(example)-pcomb-parser.opt)
EXAMPLES_ML := $(foreach example,$(EXAMPLES),$(example).ml)
EXAMPLES_PCOMB_ML := $(foreach example,$(EXAMPLES),$(example)_pcomb.ml)

REGRESSIONS = expr_attr1 expr_attr2 expr_attr3 expr_attr4 \
	expr_attr5 expr_attr6 expr_attr7 expr_attr8 \
        expr_replay1 expr_replay4 expr_replay5

REGRESSIONS_EXE := $(foreach regression,$(REGRESSIONS),$(regression)-parser)
REGRESSIONS_OPT_EXE := $(foreach regression,$(REGRESSIONS),$(regression)-parser.opt)
REGRESSIONS_PCOMB_EXE := $(foreach regression,$(REGRESSIONS),$(regression)-pcomb-parser)
REGRESSIONS_PCOMB_OPT_EXE := $(foreach regression,$(REGRESSIONS),$(regression)-pcomb-parser.opt)
REGRESSIONS_ML := $(foreach regression,$(REGRESSIONS),$(regression).ml)
REGRESSIONS_PCOMB_ML := $(foreach regression,$(REGRESSIONS),$(regression)_pcomb.ml)

ARCHIVE_base = yakker
ARCHIVE = ${ARCHIVE_base}.cma
XARCHIVE = ${ARCHIVE_base}.cmxa
PACKAGE = yakker

# Program used to time the execution of generated parsers. Default is the standard time program
TIME_EXECUTION = time -p


######################################################################

all: yak.cma yak.cmxa yakker

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
tests: $(TESTS_EXE)

.PHONY: examples
examples: $(EXAMPLES_EXE) ocaml_lex_rec

.PHONY: doc
doc: $(OCAMLDOC_SOURCES)
	$(OCAMLDOC) -html -d $(DOCDIR) -sort -t "Yakker Module Index" -short-functors $^

# GNU make hack. We turn it on to allow using a pattern-stem more than once
# in the prerequisite list. See, for example, the static pattern for the
# TESTS_ML targets.
.SECONDEXPANSION:

-include $(FRONT_END_MLL_SOURCES:.mll=.d) $(FRONT_END_ML_SOURCES:.ml=.d) $(ML_SOURCES:.ml=.d)
-include $(MLI_SOURCES:.mli=.di)
########################################################################
## Dependency fixes for yak.mli (i.e. the lack thereof)
gil.cmo gil.cmx tgraph.cmo tgraph.cmx meta_prog.cmo meta_prog.cmx: yak.cmi
variables.cmx variables.cmo : yak.cmi
rfc.cmx rfc.cmo : yak.cmi
extract_grammar.cmx extract_grammar.cmo : yak.cmi
tyspec.cmx tyspec.cmo: yak.cmi
########################################################################

yak.cmo: $(CMOS)
	@echo "--x> " $@
	@$(OCAMLC) $(OCAML_FLAGS) -g -pack -o $@ $^

yak.cmx: $(CMXS)
	@echo "--x> " $@
	@$(OCAMLOPT) $(OCAMLOPT_FLAGS) -pack -o $@ $^

$(CMXS): %.cmx: %.ml
	@echo "-x-> " $@
	@$(OCAMLOPT) $(OCAMLOPT_FLAGS) -for-pack Yak -c $< -o $@

yak.cma: yak.cmo
	@echo "--x> " $@
	@$(OCAMLC) $(OCAML_FLAGS) -g -a -o $@ -package unix $^

yak.cmxa: yak.cmx
	@echo "--x> " $@
	@$(OCAMLOPT) $(OCAMLOPT_FLAGS) -a -o $@ -package unix $^

yakkertest.cma: $(CMOS)
	@echo "--x> " $@
	@$(OCAMLC) -a  -o $@ -package unix $^

yakkertest.cmxa: $(CMXS)
	@echo "--x> " $@
	@$(OCAMLOPT) $(OCAMLOPT_FLAGS) -a -o $@ -package unix $^

universal-parser: yak.cma universal_parser.cmo
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

# Generate a dependency graph, requires (nonstandard) ocamldot
DEPEND.pdf: $(SOURCES) $(FRONT_END_SOURCES)
	touch yak.ml
	(echo yak.cmo: $(CMOS); ocamldep -I $(TOPDIR)/src -I $(TOPDIR)/batteries yak.ml $^) | ocamldot | dot -Tpdf > $@
	$(RM) yak.ml

# Qian's debugging stuff
yakker-pcomb-parser.opt: tgraph.cmx bnf.cmx yak.cmxa
	@echo "x--> yakker_pcomb.ml"
	@./yakker compile -backend fun ../examples/lex/yakker_grammar.bnf > yakker_pcomb.ml
	@echo "-x-> yakker_pcomb.cmx"
	@$(OCAMLOPT) -c yakker_pcomb.ml -o yakker_pcomb.cmx
	@echo "--x> yakker-pcomb-parser.opt"
	@$(OCAMLOPT) yak.cmxa tgraph.cmx bnf.cmx yakker_pcomb.cmx -package unix -linkpkg -o yakker-pcomb-parser.opt

yakker-lex-pcomb-parser.opt: tgraph.cmx bnf.cmx yak.cmxa
	@echo "x--> lex_pcomb.ml"
	@./yakker compile -backend fun ../examples/lex/lex_debug.bnf > lex_pcomb.ml
	@echo "-x-> lex_pcomb.cmx"
	@$(OCAMLOPT) -c lex_pcomb.ml -o lex_pcomb.cmx
	@echo "--x> lex-pcomb-parser.opt"
	@$(OCAMLOPT) yak.cmxa tgraph.cmx bnf.cmx lex_pcomb.cmx -package unix -linkpkg -o lex-pcomb-parser.opt

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
R_TESTS = expr int255 intFW scott_example3 yxml2 t000 t001 t002 t003 t004 blackbox empty eof t006 oldtyspec
R_EXAMPLES = pexpr pyexpr mailapp python
R_REGRESSIONS := $(REGRESSIONS)

C_T := $(foreach i,$(R_TESTS),$(i)-parser.opt)
C_E := $(foreach i,$(R_EXAMPLES),$(i)-parser.opt)
C_R := $(foreach i,$(R_REGRESSIONS),$(i)-parser.opt)

R_T := $(foreach i,$(R_TESTS),run-$(i))
R_E := $(foreach i,$(R_EXAMPLES),run-$(i))
R_R := $(foreach i,$(R_REGRESSIONS),run-$(i))

U_T := $(foreach i,$(R_TESTS),update-$(i))
U_E := $(foreach i,$(R_EXAMPLES),update-$(i))
U_R := $(foreach i,$(R_REGRESSIONS),update-$(i))

S_T := $(foreach i,$(R_TESTS),show-$(i))
S_E := $(foreach i,$(R_EXAMPLES),show-$(i))
S_R := $(foreach i,$(R_REGRESSIONS),show-$(i))

P_T := $(foreach i,$(R_TESTS),perf-$(i))
P_E := $(foreach i,$(R_EXAMPLES),perf-$(i)) perf-ocaml
P_R := $(foreach i,$(R_REGRESSIONS),perf-$(i))

RP_T := $(foreach i,$(R_TESTS),recperf-$(i))
RP_E := $(foreach i,$(R_EXAMPLES),recperf-$(i))
RP_R := $(foreach i,$(R_REGRESSIONS),recperf-$(i))

PC_T := $(foreach i,$(R_TESTS),pc-perf-$(i))
PC_E := $(foreach i,$(R_EXAMPLES),pc-perf-$(i)) pc-perf-ocaml
PC_R := $(foreach i,$(R_REGRESSIONS),pc-perf-$(i))

C_ALL := $(C_T)  $(C_E)  $(C_R) 
R_ALL := $(R_T)  $(R_E)  $(R_R) 
U_ALL := $(U_T)  $(U_E)  $(U_R) 
S_ALL := $(S_T)  $(S_E)  $(S_R) 
P_ALL := $(P_T)  $(P_E)  $(P_R) 
RP_ALL := $(RP_T) $(RP_E) $(RP_R)
PC_ALL := $(PC_T) $(PC_E) $(PC_R)

.PHONY: regress update-regress show-regress compile-regress perf-test record-perf-test
.PHONY: $(R_ALL) $(U_ALL) $(S_ALL) $(P_ALL) $(RP_ALL) $(PC_ALL)

regress:          $(R_ALL)
compile-regress:  $(C_ALL)
update-regress:   $(U_ALL)
show-regress:     $(S_ALL)
perf-test:        $(P_ALL)
record-perf-test: $(RP_ALL)
pc-perf-test:     $(PC_ALL)


$(R_T) $(U_T) $(S_T) $(P_T) $(PC_T) $(RP_T) : D=../tests
$(R_E) $(U_E) $(S_E) $(P_E) $(PC_E) $(RP_E) : D=../examples
$(R_R) $(U_R) $(S_R) $(P_R) $(PC_R) $(RP_R) : D=../regress

define run-regression
	@echo testing $*
	@for i in $(D)/$*/inputs/*;\
	  do ./$*-parser.opt $$i 2>&1 |\
		cmp -s - $(D)/$*/outputs/`basename $$i` || echo '    FAILURE:' `basename $$i`;\
	done
endef

define update-regression
	@echo updating regression tests for $*
	@mkdir -p $(D)/$*/outputs
	@for i in $(D)/$*/inputs/*;\
	  do ./$*-parser.opt $$i 2>&1 |\
		cmp -s - $(D)/$*/outputs/`basename $$i` ||\
		echo '    updating' $* on `basename $$i` &&\
		./$*-parser.opt $$i > $(D)/$*/outputs/`basename $$i` 2>&1 ;\
	done
endef

define show-regression
	@echo '***********************************' $* '***********************************'
	@for i in $(D)/$*/inputs/*;\
	  do ./$*-parser.opt $$i ;\
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

$(R_ALL): run-% : %-parser.opt
	$(run-regression)

$(U_ALL): update-% : %-parser.opt
	$(update-regression)

$(S_ALL): show-% : %-parser.opt
	$(show-regression)

$(P_ALL): perf-% : %-parser.opt %-pcomb-parser.opt
	@($(show-perf-test))

$(PC_ALL): pc-perf-% : %-pcomb-parser.opt
	@($(show-perf-test-pc))

$(RP_ALL): recperf-% : %-parser.opt %-pcomb-parser.opt
	@($(record-perf-test))

############################## PATTERNS ##############################

%.d: %.ml
	@echo "---> " $@
	@(cd $(dir $<); ocamldep -I ../$(OBJDIR) $(notdir $<)) >$@ # build for generated files like ocaml_lexer.ml

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
## TODO: use mktemp
########################################

.PHONY: update-yakker restore-yakker
.PHONY: bootstrap-yakker_grammar bootstrap-cmdline bootstrap-extract_grammar bootstrap-tyspec
.PHONY: gen-engine
.PHONY: restore-yakker_grammar restore-cmdline restore-extract_grammar restore-engine restore-tyspec

bootstrap-cmdline bootstrap-extract_grammar bootstrap-tyspec: bootstrap-%: $(SRCDIR)/syntax/%.bnf
	@echo checking bootstrapped file $*.ml
	@./yakker compile $(SRCDIR)/syntax/$*.bnf \
		| cmp -s - $(SRCDIR)/$*.ml ||\
		  (echo '    '$*': updating' && cp $(SRCDIR)/$*.ml $(SRCDIR)/prev_$*.ml && ./yakker compile $(SRCDIR)/syntax/$*.bnf > $(SRCDIR)/$*.ml)

bootstrap-yakker_grammar: bootstrap-%: $(SRCDIR)/syntax/%.bnf
	@echo checking bootstrapped file $*.ml
	@./yakker compile -arrow-notation $(SRCDIR)/syntax/$*.bnf \
                | perl -p -e 's/# ([0-9]+) "[^"]*ml".*$$/# \1 "yakker_grammar_lexer.ml"/g;' -e 's/# ([0-9]+) "[^"]*mll".*$$/# \1 "yakker_grammar_lexer.mll"/g' \
		| cmp -s - $(SRCDIR)/$*.ml ||\
		(echo '    '$*': updating and normalizing line directives' && cp $(SRCDIR)/$*.ml $(SRCDIR)/prev_$*.ml \
                       && (./yakker compile -arrow-notation $(SRCDIR)/syntax/$*.bnf \
                           | perl -p -e 's/# ([0-9]+) "[^"]*ml".*$$/# \1 "yakker_grammar_lexer.ml"/g;' -e 's/# ([0-9]+) "[^"]*mll".*$$/# \1 "yakker_grammar_lexer.mll"/g' \
                           > $(SRCDIR)/$*.ml))

# Note: I've tried running m4 with -s to get better error messages
# when there are compilation errors. However, m4's method of assigning
# line numbers is worse than nothing at all. FWIW, I post-processed m4's
# output with
#   perl -p -e 's/^#line/#/g;'
# to be ocaml compatible.
M4DEF=$(M4PP) -D ESET_IMPL=hier -D DO_NULL_RET=false
M4HH=$(M4PP) -D ESET_IMPL=hierhash -D DO_NULL_RET=false
M4NR=$(M4PP) -D ESET_IMPL=hier -D DO_NULL_RET=true
M4FL=$(M4PP) -D ESET_IMPL=flat -D DO_NULL_RET=false
M4NRFL=$(M4PP) -D ESET_IMPL=flat -D DO_NULL_RET=true
gen-engine: gen-% : $(SRCDIR)/engine.ml.m4
	@echo checking generated file $*.ml
	@$(M4DEF) $< | cmp -s - $(SRCDIR)/$*.ml ||\
		(echo '    updating' $* && cp $(SRCDIR)/$*.ml $(SRCDIR)/prev_$*.ml \
                                        && $(M4DEF) $< > $(SRCDIR)/$*.ml)
	@echo checking generated file $*_hh.ml
	@$(M4HH) $< | cmp -s - $(SRCDIR)/$*_hh.ml ||\
		(echo '    updating' $*_hh && cp $(SRCDIR)/$*_hh.ml $(SRCDIR)/prev_$*_hh.ml \
                                        && $(M4HH) $< > $(SRCDIR)/$*_hh.ml)
	@echo checking generated file $*_nr.ml
	@$(M4NR) $< | cmp -s - $(SRCDIR)/$*_nr.ml ||\
		(echo '    updating' $*_nr && cp $(SRCDIR)/$*_nr.ml $(SRCDIR)/prev_$*_nr.ml \
                                        && $(M4NR) $< > $(SRCDIR)/$*_nr.ml)
	@echo checking generated file $*_fl.ml
	@$(M4FL) $< | cmp -s - $(SRCDIR)/$*_fl.ml ||\
		(echo '    updating' $*_fl && cp $(SRCDIR)/$*_fl.ml $(SRCDIR)/prev_$*_fl.ml \
                                        && $(M4FL) $< > $(SRCDIR)/$*_fl.ml)
	@echo checking generated file $*_nrfl.ml
	@$(M4NRFL) $< | cmp -s - $(SRCDIR)/$*_nrfl.ml ||\
		(echo '    updating' $*_nrfl && cp $(SRCDIR)/$*_nrfl.ml $(SRCDIR)/prev_$*_nrfl.ml \
                                        && $(M4NRFL) $< > $(SRCDIR)/$*_nrfl.ml)

update-yakker: bootstrap-yakker_grammar bootstrap-cmdline bootstrap-extract_grammar bootstrap-tyspec gen-engine

restore-yakker_grammar restore-cmdline restore-extract_grammar restore-tyspec: restore-%:
	mv $(SRCDIR)/prev_$*.ml $(SRCDIR)/$*.ml

restore-engine:
	mv $(SRCDIR)/prev_$*.ml $(SRCDIR)/$*.ml
	mv $(SRCDIR)/prev_$*_nr.ml $(SRCDIR)/$*_nr.ml
	mv $(SRCDIR)/prev_$*_fl.ml $(SRCDIR)/$*_fl.ml
	mv $(SRCDIR)/prev_$*_nrfl.ml $(SRCDIR)/$*_nrfl.ml

restore-yakker: restore-yakker_grammar restore-cmdline restore-extract_grammar restore-tyspec restore-engine

########################################

ifndef NO_YR
yakker-byte: yak.cma strings.cmo rfcs.cmo buildinfo.cmo $(FRONT_END_CMOS)
	@echo "--x> " $@
	@$(OCAMLC) $^ -g -package unix -linkpkg -o $@

yakker: yak.cmxa strings.cmx rfcs.cmx buildinfo.cmx $(FRONT_END_CMXS)
	@echo "--x> " $@
	@$(OCAMLOPT) $(OCAMLOPT_FLAGS) $^ -package unix -linkpkg -o $@
endif

########################################################################
##   Standardized Tests and Examples
########################################################################

$(TESTS_ML) $(TESTS_PCOMB_ML): D=tests
$(EXAMPLES_ML) $(EXAMPLES_PCOMB_ML): D=examples
$(REGRESSIONS_ML) $(REGRESSIONS_PCOMB_ML): D=regress
# TODO: get coroutines to work with these regressions
$(REGRESSIONS_ML) $(REGRESSIONS_PCOMB_ML): YOPTS+=-arrow-notation

t006.ml : YOPTS+=-memoize-history
$(EXAMPLES_ICS_ML): YOPTS += -inline-cs
ocamlparser_regular: YOPTS+=-memoize-history

$(TESTS_OPT_EXE) $(EXAMPLES_OPT_EXE) $(REGRESSIONS_OPT_EXE): %-parser.opt: yak.cmxa %.cmx
	@echo "--x> " $@
	@$(OCAMLOPT) $(OCAMLOPT_FLAGS) $^ -package unix -linkpkg -o $@

$(TESTS_PCOMB_OPT_EXE) $(EXAMPLES_PCOMB_OPT_EXE) $(REGRESSIONS_PCOMB_OPT_EXE): %-pcomb-parser.opt: yak.cmxa %_pcomb.cmx
	@echo "--x> " $@
	@$(OCAMLOPT) $(OCAMLOPT_FLAGS) $^ -package unix -linkpkg -o $@

$(TESTS_EXE) $(EXAMPLES_EXE) $(REGRESSIONS_EXE): %-parser: yak.cma %.cmo
	@echo "--x> " $@
	@$(OCAMLC) $(OCAML_FLAGS) $^ -g -package unix -linkpkg -o $@

$(TESTS_PCOMB_EXE) $(EXAMPLES_PCOMB_EXE) $(REGRESSIONS_PCOMB_EXE): %-pcomb-parser: yak.cma %_pcomb.cmo
	@echo "--x> " $@
	@$(OCAMLC) $(OCAML_FLAGS) $^ -g -package unix -linkpkg -o $@

$(TESTS_ML) $(EXAMPLES_ML) $(REGRESSIONS_ML): %.ml : $$(D)/$$*/$$*.bnf yakker
	@echo "x--> " $@
	@./yakker compile $(YOPTS) $< > $@

$(TESTS_PCOMB_ML) $(EXAMPLES_PCOMB_ML) $(REGRESSIONS_PCOMB_ML): %_pcomb.ml : $$(D)/$$*/$$*.bnf yakker
	@echo "x--> " $@
	@./yakker compile -backend fun $< > $@

########################################################################
##  Specialized tests
########################################################################
ifeq ($(shell whoami),yitzhakm)
OCAML_COMP_DIR=/Users/yitzhakm/sw/godi/lib/ocaml/compiler-lib
endif
ifeq ($(shell whoami),trevor)
OCAML_COMP_DIR=/home/trevor/godi/lib/ocaml/compiler-lib
endif
ifndef OCAML_COMP_DIR
OCAML_COMP_DIR:=$(realpath $(dir $(shell which ocaml))../lib/ocaml/compiler-lib)
$(warning Variable OCAML_COMP_DIR undefined. Defaulting to $(OCAML_COMP_DIR).)
endif

OCAML_COMP_INCLUDES = -I $(OCAML_COMP_DIR)

# OCaml recognizer using a lexer:
ocaml_lex_rec: $(TOPDIR)/examples/ocaml_lex_rec/olr.native
	cp $< $@

$(TOPDIR)/examples/ocaml_lex_rec/olr.native:
	(cd $(TOPDIR)/examples/ocaml_lex_rec; \
         export yk_yakker_home=$(TOPDIR); \
         export yk_ocamlcomp=$(OCAML_COMP_DIR); \
         ocamlbuild olr.native)

ocamlparser_regular-parser: OCAML_FLAGS+= \
	$(OCAML_COMP_DIR)/config.cmo \
           $(OCAML_COMP_DIR)/misc.cmo \
           $(OCAML_COMP_DIR)/clflags.cmo $(OCAML_COMP_DIR)/linenum.cmo \
           $(OCAML_COMP_DIR)/warnings.cmo \
           $(OCAML_COMP_DIR)/location.cmo \
           $(OCAML_COMP_DIR)/syntaxerr.cmo 

ocamlparser_regular-parser.opt: OCAMLOPT_FLAGS+= \
	$(OCAML_COMP_DIR)/config.cmx \
           $(OCAML_COMP_DIR)/misc.cmx \
           $(OCAML_COMP_DIR)/clflags.cmx $(OCAML_COMP_DIR)/linenum.cmx \
           $(OCAML_COMP_DIR)/warnings.cmx \
           $(OCAML_COMP_DIR)/location.cmx \
           $(OCAML_COMP_DIR)/syntaxerr.cmx

ocamlparser_regular.cmo : OCAML_FLAGS+=$(OCAML_COMP_INCLUDES)
ocamlparser_regular.cmx: OCAMLOPT_FLAGS+=$(OCAML_COMP_INCLUDES)

ocamlparser: yak.cma ocamlparser.cmo llexer.cmo opdriver.cmo
	$(OCAMLC) $(OCAML_COMP_DIR)/config.cmo \
           $(OCAML_COMP_DIR)/misc.cmo \
           $(OCAML_COMP_DIR)/clflags.cmo $(OCAML_COMP_DIR)/linenum.cmo \
           $(OCAML_COMP_DIR)/warnings.cmo \
           $(OCAML_COMP_DIR)/location.cmo \
           $(OCAML_COMP_DIR)/syntaxerr.cmo \
           $^ -package unix -linkpkg -o $@

ocamlparser.opt: yak.cmxa ocamlparser.cmx llexer.cmx opdriver.cmx
	$(OCAMLOPT) $(OCAML_COMP_DIR)/config.cmx \
           $(OCAML_COMP_DIR)/misc.cmx \
           $(OCAML_COMP_DIR)/clflags.cmx $(OCAML_COMP_DIR)/linenum.cmx \
           $(OCAML_COMP_DIR)/warnings.cmx \
           $(OCAML_COMP_DIR)/location.cmx \
           $(OCAML_COMP_DIR)/syntaxerr.cmx \
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

ocamlparser.ml: ocamlparser.bnf
	./yakker compile $< > $(TOPDIR)/src/ocamlparser.ml

%.ml: %.mll
	ocamllex $<

ocaml_lexer.mll: $(TOPDIR)/ocaml/parsing/lexer.mll $(TOPDIR)/ocaml/parsing/lexer.patch
	patch -o $@ $^

ocamllex_lexer.mll: $(TOPDIR)/ocaml/lex/lexer.mll $(TOPDIR)/ocaml/lex/lexer.patch
	patch -o $@ $^

########################
# OCaml distro's parser

oparser: opbdriver.cmo
	$(OCAMLC) $(OCAML_COMP_DIR)/config.cmo \
           $(OCAML_COMP_DIR)/misc.cmo \
           $(OCAML_COMP_DIR)/clflags.cmo $(OCAML_COMP_DIR)/linenum.cmo \
           $(OCAML_COMP_DIR)/warnings.cmo \
           $(OCAML_COMP_DIR)/location.cmo \
           $(OCAML_COMP_DIR)/syntaxerr.cmo \
           $(OCAML_COMP_DIR)/parser.cmo \
           $(OCAML_COMP_DIR)/lexer.cmo \
           $(OCAML_COMP_DIR)/parse.cmo \
           $^ -package unix -linkpkg -o $@

oparser.opt: opbdriver.cmx
	$(OCAMLOPT) $(OCAML_COMP_DIR)/config.cmx \
           $(OCAML_COMP_DIR)/misc.cmx \
           $(OCAML_COMP_DIR)/clflags.cmx $(OCAML_COMP_DIR)/linenum.cmx \
           $(OCAML_COMP_DIR)/warnings.cmx \
           $(OCAML_COMP_DIR)/location.cmx \
           $(OCAML_COMP_DIR)/syntaxerr.cmx \
           $(OCAML_COMP_DIR)/parser.cmx \
           $(OCAML_COMP_DIR)/lexer.cmx \
           $(OCAML_COMP_DIR)/parse.cmx \
           $^ -package unix -linkpkg -o $@

opbdriver.cmo : %.cmo: tests/ocamls/$$*.ml
	$(OCAMLC) $(OCAML_COMP_INCLUDES) -c $< -o $@

opbdriver.cmx : %.cmx: tests/ocamls/$$*.ml
	$(OCAMLOPT) $(OCAML_COMP_INCLUDES) -c $< -o $@


###############################

ocamlp-w-c: yak.cmxa ocamlpwc.cmx
	$(OCAMLOPT) $^ -package unix -linkpkg -o $@

ocamlpwc.ml: examples/ocaml/ocaml-with-coroutines.bnf yakker
	@echo "x--> " $@
	@./yakker compile $< > $@

ocamlp-wo-c: yak.cmxa ocamlpwoc.cmx
	$(OCAMLOPT) $^ -package unix -linkpkg -o $@

ocamlpwoc.ml: examples/ocaml/ocaml-without-coroutines.bnf yakker
	@echo "x--> " $@
	@./yakker compile $< > $@


aurochswoc-parser aurochswc-parser: %-parser: yak.cmxa %.cmx
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

#wfe : yak.cmxa ocaml_woc.cmx python2.cmx engine.ml test_wfe.ml
wfe : yak.cmxa ocaml_woc.cmx
	$(OCAMLOPT) -package unix -linkpkg -inline 30 $^ -o $@

wfe_g : yak.cma ocaml_woc.cmo python2.cmo engine.ml test_wfe.ml
	$(OCAMLC) -g -package unix -linkpkg $^ -o $@


########################################################################
##  yakker component tests
########################################################################

test_ykBuf test_cs : test_% : yak.cmxa test_%.cmx
	$(OCAMLOPT) -package unix -linkpkg $^ -o $@

########################################################################

aurochs-tx: yak.cma tgraph.cmo bnf.cmo cs.cmo pr.cmo aurochs.cmo aurochs_tx.cmo
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
