include $(top_srcdir)/rules/app.mk

OTP_APP = yate
yate_VSN = 0.1.0

yate_SRCS = \
key_dict.erl \
key_list.erl \
yate_app.erl \
yate_call.erl \
yate_call_reg.erl \
yate_command.erl \
yate_conn.erl \
yate_conn_srv.erl \
yate_decode.erl \
yate_encode.erl \
yate.erl \
yate_notify.erl \
yate_pending.erl \
yate_port_srv.erl \
yate_reg_srv.erl \
yate_srv.erl \
yate_sup.erl \
yate_test.erl \
yate_util.erl

AM_ERLCFLAGS = -I$(srcdir)/../include
#AM_ERLCFLAGS = -I$(top_srcdir)/include
#AM_ERL_FLAGS = -pa $(top_builddir)/releases

run:
	$(ERL) $(AM_ERL_FLAGS) $(ERL_FLAGS) -name erlyate -boot yate -sasl sasl_error_logger '{file,"sasl.log"}' -mnesia dir '"/var/tmp/mnesia/erlyate"' -s appmon start
