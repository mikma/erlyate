
run:
	$(ERL) $(AM_ERL_FLAGS) $(ERL_FLAGS) -name erlyate -boot sipclient -sasl sasl_error_logger '{file,"sasl.log"}' -mnesia dir '"/var/tmp/mnesia/erlyate"' -yxa_config sipclient.config -s appmon start
#	$(ERL) -boot yate -sasl sasl_error_logger '{file,"sasl.log"}'
