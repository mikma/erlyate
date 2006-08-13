%%%
%%% @doc       Interface module
%%% @author    Mikael Magnusson <mikma@users.sourceforge.net>
%%% @copyright 2006 Mikael Magnusson
%%%
-module(yate).

%% api
%%-export([connect/2, stop/1, install/2, install/3, uninstall/2, watch/2, unwatch/2, ret/3, ret/4, queue_msg/3, send_msg/3]).
-export([connect/2, open/1, install/3, uninstall/2, watch/3, unwatch/2, ret/3, ret/4, queue_msg/4, send_msg/3]).

-include("yate.hrl").

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
%% @spec link(Conn) -> Result
%%           Result = {ok, Handle} | {error, Reason}
%% @doc Link to Yate extmodule
%% @end
%%--------------------------------------------------------------------
open(Client) ->
    UserPid = self(),
    {ok, {yate_client, Client, UserPid}}.

%%--------------------------------------------------------------------
%% @spec watch(Handle, Name, Fun) -> true
%%           Handle = pid()
%%           Name = atom()
%%           Fun = fun(Cmd) -> true | false
%%           Cmd = #command{}
%% @doc install message watcher (post-dispatching notifier)
%% @end
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
%% @spec install(Handle, Name, Fun) -> ok
%%           Handle = pid()
%%           Name = atom()
%%           Fun = fun(Cmd) -> true | false
%%           Cmd = #command{}
%% @doc install message handler
%% @end
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
%% @doc answer message
%% @end
%%--------------------------------------------------------------------
ret(Handle, Cmd, Processed) ->
    Header = (Cmd#command.header)#message{processed=Processed},
    cast(Handle, {ret, Cmd#command{header=Header}}).

%%--------------------------------------------------------------------
%% @spec ret(Handle, Cmd, Processed, Retval) -> ok
%%           Handle = pid()
%%           Cmd = #command{}
%%           Retval = string()
%% @doc answer message
%% @end
%%--------------------------------------------------------------------
ret(Handle, Cmd, Processed, Retval) ->
    Header = (Cmd#command.header)#message{processed=Processed},
    cast(Handle, {ret, Cmd#command{retvalue=Retval,header=Header}}).

%%--------------------------------------------------------------------
%% @spec queue_msg(Handle, Name, Keys) -> ok
%%           Handle = pid()
%%           Name = string()
%%           Keys = dictionary()
%% @doc post message without waiting for answer
%% @end
%%--------------------------------------------------------------------
queue_msg(Handle, Name, Keys, Tag) ->
    call(Handle, {msg, Name, Keys, Tag}).


%%--------------------------------------------------------------------
%% @spec msg(Handle, Name, Keys) -> {ok, Cmd}
%%           Handle = pid()
%%           Name = string()
%%           Keys = dictionary()
%% @doc send message and wait for answer
%% @end
%%--------------------------------------------------------------------
send_msg(Handle, Name, Keys) ->
    call(Handle, {msg, Name, Keys}).


%%--------------------------------------------------------------------
%% @spec stop(Handle) -> ok
%%           Handle = pid()
%% @doc close connection
%% @end
%%--------------------------------------------------------------------
%% stop(Handle) ->
%%     gen_server:cast(Handle, stop).


%%
%% private functions
%%
call({yate_client, Handle, UserPid}, Request) ->
    gen_server:call(Handle, {client, Request, UserPid}).

cast({yate_client, Handle, UserPid}, Request) ->
    gen_server:cast(Handle, {client, Request, UserPid}).
