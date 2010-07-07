%%%
%%% @doc       Interface module
%%% @author    Mikael Magnusson <mikma@users.sourceforge.net>
%%% @copyright 2006-2008 Mikael Magnusson
%%%
-module(yate).

%% api
%%-export([connect/2, stop/1, install/2, install/3, uninstall/2, watch/2, unwatch/2, ret/3, ret/4, queue_msg/3, send_msg/3]).
-export([connect/2, open/1, dup/1, close/1, install/2, install/3, uninstall/2, watch/3, unwatch/2, ret/3, ret/4, queue_msg/4, send_msg/3]).

-export([start/0]).

-include("yate.hrl").

%%--------------------------------------------------------------------
%% @spec start() -> Result
%% @doc Start yate app and its dependent application(s).
%% @end
%%--------------------------------------------------------------------
start() ->
    application:start(sasl),
    application:start(yate),
    ok.

%%--------------------------------------------------------------------
%% @spec connect(Host, Port) -> Result
%%           Host = string() | atom() | ip_address()
%%           Port = integer()
%%           Result = {ok, Handle} | {error, Reason}
%% @doc Connect to Yate extmodule
%% @end
%%--------------------------------------------------------------------
connect(Host, Port) ->
    yate_sup:start_client(Host, Port).

%%--------------------------------------------------------------------
%% @spec open(Client) -> Result
%%           Result = {ok, Handle} | {error, Reason}
%% @doc Link to Yate extmodule
%% @end
%%--------------------------------------------------------------------
open(Client) ->
    UserPid = self(),
    link(Client),
    {ok, {yate_client, Client, UserPid}}.

dup({yate_client, Client, _UserPid}) ->
    UserPid = self(),
    link(Client),
    {ok, {yate_client, Client, UserPid}}.

close(Handle) ->
    call(Handle, close).

%%--------------------------------------------------------------------
%% @spec watch(Handle, Name, Fun) -> true
%%           Handle = pid()
%%           Name = atom()
%%           Fun = fun()
%%           Cmd = #command{}
%% @doc install message watcher (post-dispatching notifier)
%% @end
%% TODO: Fun = fun(Cmd) -> true | false
%%--------------------------------------------------------------------
watch(Handle, Name, Fun) ->
    call(Handle, {watch, Name, Fun}).

%%--------------------------------------------------------------------
%% @spec unwatch(Handle, Name) -> true
%%           Handle = pid()
%%           Name = atom()
%% @doc uninstall message watcher (post-dispatching notifier)
%% @end
%%--------------------------------------------------------------------
unwatch(Handle, Name) ->
    call(Handle, {unwatch, Name}).

%%--------------------------------------------------------------------
%% @spec install(Handle, Name) -> ok
%%           Handle = pid()
%%           Name = atom()
%%           Cmd = #command{}
%% @doc install message handler
%% @end
%%--------------------------------------------------------------------
install(Handle, Name) ->
    install(Handle, Name, fun true_func/1).

%%--------------------------------------------------------------------
%% @spec install(Handle, Name, Fun) -> ok
%%           Handle = pid()
%%           Name = atom()
%%           Fun = fun()
%%           Cmd = #command{}
%% @doc install message handler
%% @end
%% TODO: Fun = fun(Cmd) -> true | false
%%--------------------------------------------------------------------
install(Handle, Name, Fun) ->
    call(Handle, {install, Name, Fun}).

%%--------------------------------------------------------------------
%% @spec uninstall(Handle, Name) -> ok
%%           Handle = pid()
%%           Name = atom()
%% @doc uninstall message handler
%% @end
%%--------------------------------------------------------------------
uninstall(Handle, Name) ->
    call(Handle, {uninstall, Name}).

%%--------------------------------------------------------------------
%% @spec ret(Handle, Cmd, Processed) -> ok
%%           Handle = pid()
%%           Cmd = #command{}
%%           Processed = boolean()
%% @doc answer message
%% @end
%%--------------------------------------------------------------------
ret(Pid, Cmd, Processed) when is_pid(Pid), is_record(Cmd, command),
                            is_boolean(Processed) ->
    Pid ! {ret, Cmd#command{success=Processed}},
    ok.

%%--------------------------------------------------------------------
%% @spec ret(Handle, Cmd, Processed, Retval) -> ok
%%           Handle = pid()
%%           Cmd = #command{}
%%           Processed = boolean()
%%           Retval = string()
%% @doc answer message
%% @end
%%--------------------------------------------------------------------
ret(Pid, Cmd, Processed, Retval) when is_pid(Pid), is_record(Cmd, command),
                                    is_boolean(Processed) ->
    Header = (Cmd#command.header)#message{retvalue=Retval},
    Pid ! {ret, Cmd#command{success=Processed,header=Header}},
    ok.

%%--------------------------------------------------------------------
%% @spec queue_msg(Handle, Name, Keys, Tag) -> ok
%%           Handle = pid()
%%           Name = string()
%%           Keys = dictionary()
%% @doc post message without waiting for answer
%% @end
%%--------------------------------------------------------------------
queue_msg(Handle, Name, Keys, Tag) ->
    call(Handle, {msg, Name, Keys, Tag}).


%%--------------------------------------------------------------------
%% @spec send_msg(Handle, Name, Keys) -> {ok, Cmd}
%%           Handle = pid()
%%           Name = string()
%%           Keys = dictionary()
%% @doc send message and wait for answer
%% @end
%%--------------------------------------------------------------------
send_msg(Handle, Name, Keys) ->
    call(Handle, {msg, Name, Keys}).


%%
%% private functions
%%
call({yate_client, Handle, UserPid}, Request) ->
    gen_server:call(Handle, {client, Request, UserPid}).

cast({yate_client, Handle, UserPid}, Request) ->
    gen_server:cast(Handle, {client, Request, UserPid}).

true_func(_Cmd) ->
    true.
