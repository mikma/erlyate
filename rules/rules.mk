SUBST = sed $(ERLANG_LIB_VER_SUBST) \
	-e 's,[@]VERSION[@],$(VERSION),g' \
	-e 's,[@]ERLANG_ERTS_VER[@],$(ERLANG_ERTS_VER),g' \
	-e 's/[@]$(OTP_APP)_VSN[@]/$($(OTP_APP)_VSN)/'

%.rel: %.rel.in $(top_builddir)/config.status
	$(SUBST) $< > $@

%.beam: %.erl
	@echo [ERLC] $@
	@$(ERLC) $(AM_ERL_FLAGS) $(ERL_FLAGS) $(AM_ERLCFLAGS) $(ERLCFLAGS) $<

%.app: %.app-in $(top_builddir)/config.status
	$(SUBST) $< > $@

%.boot: %.rel
	@echo [ERLC] $@
	@$(ERLC) $(AM_ERL_FLAGS) $(ERL_FLAGS) $(AM_ERLCFLAGS) $(ERLCFLAGS) $<
