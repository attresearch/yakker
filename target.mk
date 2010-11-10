.SUFFIXES:

ifndef OBJDIR
  OBJDIR := build
  $(info Defaulting to $(OBJDIR) as build directory)
endif

MAKETARGET = $(MAKE) --no-print-directory -C $@ -f $(CURDIR)/Makefile TOPDIR=$(CURDIR) $(MAKECMDGOALS)

.PHONY: $(OBJDIR)
$(OBJDIR):
	+@[ -d $@ ] || mkdir -p $@
	+@$(MAKETARGET)

Makefile : ;
%.mk :: ;

% :: $(OBJDIR) ; :

.PHONY: clean
clean:
	$(RM) -r $(OBJDIR)
