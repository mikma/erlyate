%%%
%%% @doc       TCP connection server
%%% @author    Mikael Magnusson <mikma@users.sourceforge.net>
%%% @copyright 2006-2008 Mikael Magnusson
%%%
-module(yate_conn).

-include("yate.hrl").

-behaviour(gen_server).

%% api
-export([start_link/3, stop/1]).
-export([install/2, install/3, uninstall/2, watch/2, unwatch/2, ret/3, ret/4, queue_msg/4, send_msg/3]).

%% gen_server callbacks
-export([init/1,
	 code_change/3,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2]).

%% pending: Dictionary of {Type, Id} -> From
-record(sstate, {sock, pid, queue, pending=dict:new(), installed=dict:new(),
		 watched=dict:new()}).

-define(SERVER, ?MODULE).


%%--------------------------------------------------------------------
%% @spec start_link(Host, Port, Pid) -> Result
%%           Host = string()
%%           Port = integer()
%%           Result = {ok, Pid} | {error, Reason}
%% @doc Start server and connect to Yate as external module
%% @end
%%--------------------------------------------------------------------
start_link(Host, Port, Pid) ->
    gen_server:start_link(?MODULE, [Host, Port, Pid], [{timeout, 1000}]).


%%--------------------------------------------------------------------
%% @spec stop(Handle) -> ok
%%           Handle = pid()
%% @doc close connection
%% @end
%%--------------------------------------------------------------------
stop(Handle) ->
    gen_server:cast(Handle, stop).

%%--------------------------------------------------------------------
%% @spec watch(Handle, Name) -> true
%%           Handle = pid()
%%           Name = atom()
%% @doc install message watcher (post-dispatching notifier)
%% @end
%%--------------------------------------------------------------------
watch(Handle, Name) ->
    call(Handle, {watch, Name}).

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
%% @doc install message handler
%% @end
%%--------------------------------------------------------------------
install(Handle, Name) ->
    install(Handle, Name, 10).

%%--------------------------------------------------------------------
%% @spec install(Handle, Name, Priority) -> ok | error
%%           Handle = pid()
%%           Name = atom()
%%           Priority = integer()
%% @doc install message handler
%% @end
%%--------------------------------------------------------------------
install(Handle, Name, Priority) ->
    call(Handle, {install, Name, Priority}).


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
%% @spec ret(Handle, Cmd, Success) -> ok
%%           Handle = pid()
%%           Cmd = #command{}
%% @doc answer message
%% @end
%%--------------------------------------------------------------------
ret(Handle, Cmd, Success) ->
    gen_server:cast(Handle, {ret, Cmd#command{success=Success}}).

%%--------------------------------------------------------------------
%% @spec ret(Handle, Cmd, Processed, Retval) -> ok
%%           Handle = pid()
%%           Cmd = #command{}
%%           Retval = string()
%% @doc answer message
%% @end
%%--------------------------------------------------------------------
ret(Handle, Cmd, Success, Retval) ->
    Header = (Cmd#command.header)#message{retvalue=Retval},
    gen_server:cast(Handle, {ret, Cmd#command{success=Success,header=Header}}).

%%--------------------------------------------------------------------
%% @spec queue_msg(Handle, Name, Keys, Tag) -> ok
%%           Handle = pid()
%%           Name = string()
%%           Keys = dictionary()
%% @doc post message without waiting for answer
%% @end
%%--------------------------------------------------------------------
queue_msg(Handle, Name, Keys, Tag) ->
    gen_server:cast(Handle, {msg, Name, Keys, self(), Tag}).


%%--------------------------------------------------------------------
%% @spec send_msg(Handle, Name, Keys) -> {ok, Cmd}
%%           Handle = pid()
%%           Name = string()
%%           Keys = dictionary()
%% @doc send message and wait for answer
%% @end
%%--------------------------------------------------------------------
send_msg(Handle, Name, Keys) ->
    gen_server:call(Handle, {msg, Name, Keys}).


%%
%% private functions
%%
call(Handle, Request) ->
    Retvalue =
        case gen_server:call(Handle, Request) of
            {error, Reason} ->
                throw(Reason);
            Retvalue2 ->
                Retvalue2
        end,
    Retvalue.


%%
%% gen_server callbacks
%%
%% @private
init([Host, Port, Pid]) ->
    init([Host, Port, Pid, []]);
init([Host, Port, Pid, Options]) ->
    error_logger:info_msg("Start ~p ~p~n", [?MODULE, self()]),
    NewOptions = Options ++ [list, {packet, line}],
    {ok, Sock} = do_connect(Host, Port, NewOptions),
    Header = #connect{role=global,type=""},
    Cmd = #command{id=erl,header=Header,type=connect},
    ok = send_command(Sock, req, Cmd),
    {ok, #sstate{sock=Sock,pid=Pid}}.

%%
%% @doc retry connect if refused until it succeeds
do_connect(Host, Port, Options) ->
    case gen_tcp:connect(Host, Port, Options) of
	{ok, Sock} ->
	    {ok, Sock};
	{error,econnrefused} ->
	    receive
	    after
		1000 ->
		    do_connect(Host, Port, Options)
	    end
    end.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% @private
handle_call({install, Name, Prio}, From, State) ->
    case dict:is_key(Name, State#sstate.installed) of
	false ->
	    Header = #install_req{priority=Prio},
	    Cmd = #command{id=Name,header=Header,type=install},
	    {ok, NewState} = queue_command(req, Cmd, {call, From}, State),
	    NewInstalled = dict:append(Name, Prio, State#sstate.installed),
	    {noreply, NewState#sstate{installed=NewInstalled}};
	true ->
	    {reply, ok, State}
    end;
handle_call({uninstall, Name}, From, State) ->
    Installed = State#sstate.installed,
    case dict:is_key(Name, Installed) of
	true ->
	    Cmd = #command{id=Name,type=uninstall},
	    {ok, NewState} = queue_command(req, Cmd, {call, From}, State),
	    NewInstalled = dict:erase(Name, Installed),
	    {noreply, NewState#sstate{installed = NewInstalled}};
	false ->
	    {reply, ok, State}
    end;
handle_call({watch, Name}, From, State) ->
    case dict:is_key(Name, State#sstate.watched) of
	false ->
	    Cmd = #command{id=Name,type=watch},
	    {ok, NewState} = queue_command(req, Cmd, {call, From}, State),
	    NewDict = dict:append(Name, From, State#sstate.watched),
	    {noreply, NewState#sstate{watched=NewDict}};
	true ->
	    {reply, ok, State}
    end;
handle_call({unwatch, Name}, From, State) ->
    Dict = State#sstate.watched,
    case dict:is_key(Name, Dict) of
	true ->
	    Cmd = #command{id=Name,type=unwatch},
	    {ok, NewState} = queue_command(req, Cmd, {call, From}, State),
	    NewDict = dict:erase(Name, Dict),
	    {noreply, NewState#sstate{watched = NewDict}};
	false ->
	    {reply, ok, State}
    end;
handle_call({msg, Name, Keys}, From, State) ->
    {ok, NewState} = queue_message(Name, Keys, {call, From}, State),
    {noreply, NewState};
handle_call(Request, _From, State) ->
    error_logger:error_msg("Unhandled call ~p: ~p~n", [?MODULE, Request]),
    {reply, ok, State}.


%% @private
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast({ret, Cmd}, State) ->
    Sock = State#sstate.sock,
    ok = send_command(Sock, ans, Cmd),
    {noreply, State};
handle_cast({msg, Name, Keys, Pid, Tag}, State) ->
    {ok, NewState} = queue_message(Name, Keys, {cast, Pid, Tag}, State),
    {noreply, NewState};
handle_cast(Request, State) ->
    error_logger:error_msg("Unhandled cast in ~p: ~p~n", [?MODULE, Request]),
    {noreply, State}.


%% @private
handle_info({tcp, _Socket, Data}, State) ->
    {ok, NewState} = handle_tcp(Data, State),
    {noreply, NewState};
handle_info({tcp_closed, Socket}, State) ->
    error_logger:info_msg("stop TCP closed~n"),
    {stop, {tcp_closed, Socket}, State};
handle_info({tcp_error, _Socket, Reason}, State) ->
    error_logger:info_msg("TCP error: ~p~n", [Reason]),
    {noreply, State};
handle_info(Info, State) ->
    error_logger:error_msg("Unhandled info in ~p: ~p~n", [?MODULE, Info]),
    {noreply, State}.


%% @private
terminate(Reason, _State) ->
    error_logger:error_msg("~p terminated ~p~n", [?MODULE, Reason]),
    terminated.


queue_message(Name, Keys, Tag, State) ->
    Time = yate_util:seconds(),
    Header = #message{time=Time,name=Name,retvalue=""},
    Id = erlang:ref_to_list(make_ref()),
    Cmd = #command{id=Id,type=message,header=Header},
    Cmd1 = yate_command:append_keys(Keys, Cmd),
    {ok, NewState} = queue_command(req, Cmd1, Tag, State),
    {ok, NewState}.


queue_command(Dir, Cmd, Tag, State) ->
    ok = send_command(State#sstate.sock, Dir, Cmd),
    Key = {Cmd#command.type, Cmd#command.id},
    Pending = dict:store(Key, Tag, State#sstate.pending),
    {ok, State#sstate{pending=Pending}}.


handle_tcp(Data, State) ->
    {ok, Dir, Cmd} = yate_decode:decode_command(Data),
    {ok, NewState} = handle_command(Dir, Cmd, State),
    {ok, NewState}.

handle_command(ans, Cmd, State) ->
    Id = Cmd#command.id,
    Key = {Cmd#command.type, Id},
    case dict:find(Key, State#sstate.pending) of
	{ok, Tag} ->
	    Reply =
                case Cmd#command.success of
                    true -> ok;
                    false -> {error, failure}
                end,
	    case Tag of
		{call, From} ->
		    gen_server:reply(From, Reply);
		{cast, Pid, Tag2} ->
		    Pid ! {cast, {ans, Reply, Cmd}, Tag2};
		_ ->
		    error_logger:error_msg("handle_command wrong tag ~p~n",
					   [Tag])
	    end,
	    Pending = dict:erase(Key, State#sstate.pending),
	    {ok, State#sstate{pending=Pending}};
	error ->
	    Pid = State#sstate.pid,
	    Pid ! {yate, ans, Cmd, self()},
	    {ok, State}
    end;
handle_command(req, Cmd, State) when Cmd#command.type == message ->
    Pid = State#sstate.pid,
    Pid ! {yate, req, Cmd, self()},
    {ok, State};
handle_command(req, Cmd, State) ->
    error_logger:error_msg("Unhandled request in ~p: ~p~n", [?MODULE, Cmd]),
    {ok, State}.


send_command(Sock, Dir, Cmd) ->
    {ok, Msg} = yate_encode:encode_command(Cmd#command.type, Dir, Cmd),
    ok = gen_tcp:send(Sock, Msg).
