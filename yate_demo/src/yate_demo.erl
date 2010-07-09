%%%
%%% @doc       Demo
%%% @author    Mikael Magnusson <mikma@users.sourceforge.net>
%%% @copyright 2006 Mikael Magnusson
%%%
-module(yate_demo).

-behaviour(gen_server).

-include("yate.hrl").

%% api
-export([start_link/0, stop/0, run/0]).

%% gen_server callbacks
-export([init/1,
	 code_change/3,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2]).

-record(state, {handle, client}).

-define(SERVER, ?MODULE).
-define(HOST, localhost).
-define(PORT, 15062).


%%--------------------------------------------------------------------
%% @spec run() -> ok
%% @doc run demo
%% @end
%%--------------------------------------------------------------------
run() ->
    yate_sup:start_link(),
    {ok, _Pid} = start_link(),
    ok.

    
%%--------------------------------------------------------------------
%% @spec start_link() -> Result
%%           Result = {ok, Pid} | ignore | {error, Error}
%%           Pid = pid()
%%           Error = {already_started, Pid} | term()
%% @doc create demo process
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], [{timeout, 5000}]).


%%--------------------------------------------------------------------
%% @spec stop() -> ok
%% @doc stop demo process
%% @end
%%--------------------------------------------------------------------
stop() ->
    gen_server:cast(?SERVER, stop).

%%
%% gen_server callbacks
%%
init([]) ->
    error_logger:info_msg("start ~p~n", [?MODULE]),
    {ok, Configdir} = application:get_env(yate, yateconfdir),
    Config = yate_config:read(filename:join(Configdir, "extmodule.conf")),
    {tcp, Host, Port} = yate_config:get_extmodule_listener(Config, "erlyate"),
    {ok, Client} = yate:connect(Host, Port),
    {ok, Handle} = yate:open(Client),
    ok = yate:install(Handle, call.route),
    ok = yate:install(Handle, call.execute),
    {ok, #state{handle=Handle, client=Client}}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_call(Request, _From, _State) ->
    exit({unhandled, request, Request}).


handle_cast(stop, State) ->
    {stop, normal, State}.


handle_info({yate_req, Cmd, From}, State) ->
    handle_command(Cmd#command.type, req, Cmd, From, State);
handle_info({yate_ans, Cmd, From}, State) ->
    handle_command(Cmd#command.type, ans, Cmd, From, State);
handle_info({cast, {ans, RetValue, RetCmd}, {call, From}}, State) ->
    Id = yate_command:fetch_key(id, RetCmd),
    error_logger:info_msg("Result ~p ~p~n", [RetValue, Id]),
    gen_server:reply(From, RetValue),
    {noreply, State};
handle_info({'EXIT', Pid, Reason}, State) ->
    error_logger:error_msg("EXIT: ~p ~p~n", [Pid, Reason]),
    {noreply, State}.


terminate(_Reason, State) ->
    Handle = State#state.handle,
    yate:uninstall(Handle, call.route),
    yate:uninstall(Handle, call.execute),
    yate:close(Handle),
    terminated.


handle_command(message, req, Cmd, From, State) ->
    case handle_message((Cmd#command.header)#message.name, Cmd, From, State) of
	{noreply, State1} ->
	    {noreply, State1};
	{reply, {Success, Retval}, State1} ->
	    ok = yate:ret(From, Cmd, Success, Retval),
	    {noreply, State1};
	{reply, {Success, Retval, Cmd1}, State1} ->
	    ok = yate:ret(From, Cmd1, Success, Retval),
	    {noreply, State1};
	{reply, Success, State1} when Success == false; Success == true->
	    ok = yate:ret(From, Cmd, Success),
	    {noreply, State1};
	{stop, Reason, State1} ->
	    {stop, Reason, State1}
    end;

handle_command(message, ans, _Cmd, _From, State) ->
    error_logger:info_msg("Ignore answer/watch.~n", []),
    {noreply, State}.


handle_message(call.execute, Cmd, From, State) ->
    Callto = yate_command:fetch_key(callto, Cmd),
    handle_call_execute(Callto, Cmd, From, State);
handle_message(call.route, Cmd, From, State) ->
    handle_call_route(yate_command:fetch_key(called, Cmd), Cmd, From, State).


handle_call_execute("erl/" ++ String, Cmd, From, State) ->
    [File, FuncStr | Args] = string:tokens(String, "/"),

    ModuleName = list_to_atom(File),
    Func = list_to_atom(FuncStr),
    error_logger:info_msg("Handle Call execute ~p ~p ~p.~n", [ModuleName, Func, Args]),
    case catch apply(ModuleName, Func,
		     [State#state.client, Cmd, From, Args]) of
	{'EXIT', Term} ->
	    error_logger:error_msg("call.execute failed ~p.~n", [Term]),
	    yate:ret(From, Cmd, false);
	_ ->
	    ok
    end,
    {noreply, State};
handle_call_execute(_Called, _Cmd, _From, State) ->
%%     error_logger:info_msg("Unhandled call.execute to: ~p~n", [Called]),
    {reply, false, State}.


handle_call_route("demo", _Cmd, _From, State) ->
    {reply, {true, "erl/yate_demo_call/start_link"}, State};
handle_call_route("clock", _Cmd, _From, State) ->
    {reply, {true, "erl/yate_clock/start_link"}, State};
handle_call_route("y600", _Cmd, _From, State) ->
    {reply, {true, "erl/sipclient/call/sip:600@mulder"}, State};
%% handle_call_route("sftf", _Cmd, _From, State) ->
%%     %% SFTF test framework
%%     {reply, {true, "erl/sipclient/call/sip:sftf@192.168.0.4:5066"}, State};
%% handle_call_route("sftf-tcp", _Cmd, _From, State) ->
%%     %% SFTF test framework
%%     {reply, {true, "erl/sipclient/call/sip:sftf@192.168.0.4:5066;transport=tcp"}, State};
handle_call_route(Called, _Cmd, _From, State) ->
    error_logger:error_msg("Unhandled call.route to: ~p~n", [Called]),
    {reply, false, State}.
