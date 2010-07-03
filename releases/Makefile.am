include $(top_srcdir)/rules/rel.mk

OTP_RELEASES = yate yate_demo sipclient

AM_ERL_FLAGS = -pa $(top_builddir)/yate/ebin -pa			\
$(top_builddir)/yate_demo/ebin -pa $(top_builddir)/sipclient/ebin

run:
	YATE=$(YATE) $(ERL) $(AM_ERL_FLAGS) $(ERL_FLAGS) -name yate

run-yatedemo:
	YATE=$(YATE) $(ERL) $(AM_ERL_FLAGS) $(ERL_FLAGS) -name yate -boot yate_demo -sasl sasl_error_logger '{file,"sasl.log"}'

run-sipclient:
	$(ERL) $(AM_ERL_FLAGS) $(ERL_FLAGS) -name yate -boot sipclient -sasl sasl_error_logger '{file,"sasl.log"}' -mnesia dir '"/var/tmp/mnesia/erlyate"' -yxa_config sipclient.config -s tv start


