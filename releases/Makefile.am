include $(top_srcdir)/rules/rel.mk

OTP_RELEASES = yate yate_demo sipclient

AM_ERL_FLAGS = -pa $(top_builddir)/yate/ebin -pa			\
$(top_builddir)/yate_demo/ebin -pa $(top_builddir)/sipclient/ebin

run:
	$(ERL) $(AM_ERL_FLAGS) $(ERL_FLAGS) -name yate
# -s systools make_tar sipb2bua -s init exit

run-yatedemo:
	$(ERL) $(AM_ERL_FLAGS) $(ERL_FLAGS) -boot yate_demo -name yate

run-sipclient:
	$(ERL) $(AM_ERL_FLAGS) $(ERL_FLAGS) -name eryate -boot sipclient -sasl sasl_error_logger '{file,"sasl.log"}' -mnesia dir '"/var/tmp/mnesia/eryate"' -yxa_config sipclient.config -s tv start
