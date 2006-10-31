libsubdir = $(ERLANG_INSTALL_LIB_DIR)/$(OPT_APP)-$($(OPT_APP)_VSN)
bindir = $(libsubdir)/bin
ebindir = $(libsubdir)/ebin

beam_FILES = $($(OPT_APP)_SRCS:.erl=.beam)

ebin_DATA = $(beam_FILES) $(OPT_APP:=.app) $(OPT_APP:=.boot)
EXTRA_DIST = $($(OPT_APP)_SRCS) $(OPT_APP:=.app-in) $(OPT_APP:=.rel.in)
CLEANFILES = $(beam_FILES) $(OPT_APP:=.app) $(OPT_APP:=.boot) $(OPT_APP:=.rel)	\
$(OPT_APP:=.script)

SUBST = sed \
	-e 's,[@]ERLANG_LIB_VER_kernel[@],$(ERLANG_LIB_VER_kernel),g' \
	-e 's,[@]ERLANG_LIB_VER_stdlib[@],$(ERLANG_LIB_VER_stdlib),g' \
	-e 's,[@]ERLANG_LIB_VER_sasl[@],$(ERLANG_LIB_VER_sasl),g'

%.rel: %.rel.in
	$(SUBST) $< > $@

%.beam: %.erl
	$(ERLC) $(AM_ERLFLAGS) $(ERLFLAGS) $(AM_ERLCFLAGS) $(ERLCFLAGS) $<

%.app: %.app-in
	cp $< $@

%.boot: %.rel %.app
	$(ERLC) $(AM_ERLFLAGS) $(ERLFLAGS) $(AM_ERLCFLAGS) $(ERLCFLAGS) $<
