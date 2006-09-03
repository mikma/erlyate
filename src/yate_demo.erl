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
    {ok, Client} = yate:connect(?HOST, ?PORT),
    {ok, Handle} = yate:open(Client),
    ok = yate:install(Handle, call.route, 
		      fun(_Cmd) ->
			      true
		      end),
    ok = yate:install(Handle, call.execute,
		      fun(_Cmd) ->
			      true
		      end),
    ok = yate:watch(Handle, chan.hangup,
		      fun(_Cmd) ->
			      true
		      end),
    ok = yate:watch(Handle, chan.disconnected,
		      fun(_Cmd) ->
			      true
		      end),
    ok = yate:watch(Handle, chan.startup,
		      fun(_Cmd) ->
			      true
		      end),
    {ok, #state{handle=Handle, client=Client}}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_call(Request, _From, _State) ->
    exit({unhandled, request, Request}).


handle_cast(stop, State) ->
    {stop, normal, State}.


handle_info({yate, Dir, Cmd, From}, State) ->
    handle_command(Cmd#command.type, Dir, Cmd, From, State);
handle_info({'EXIT', Pid, Reason}, State) ->
    error_logger:error_msg("EXIT: ~p ~p~n", [Pid, Reason]),
    {noreply, State}.


terminate(_Reason, State) ->
    Handle = State#state.handle,
    yate:uninstall(Handle, call.route),
    yate:uninstall(Handle, call.execute),
    terminated.


handle_command(message, req, Cmd, From, State) ->
    handle_message((Cmd#command.header)#message.name, Cmd, From, State);
handle_command(message, ans, _Cmd, _From, State) ->
    error_logger:info_msg("Ignore answer/watch.~n", []),
    {noreply, State}.


handle_message(call.execute, Cmd, From, State) ->
    Callto = dict:fetch(callto, Cmd#command.keys),
    error_logger:info_msg("Handle Call execute ~p.~n", [Callto]),
    handle_call_execute(Callto, Cmd, From, State);
handle_message(call.route, Cmd, From, State) ->
    handle_call_route(dict:fetch(called, Cmd#command.keys), Cmd, From, State).


handle_call_execute("erl/" ++ String, Cmd, From, State) ->
    [File, FuncStr | Args] = string:tokens(String, "/"),

    ModuleName = list_to_atom(File),
    Func = list_to_atom(FuncStr),
    case catch apply(ModuleName, Func,
		     [State#state.client, Cmd, From, Args]) of
	{'EXIT', Term} ->
	    error_logger:error_msg("call.execute failed ~p.~n", [Term]),
	    yate:ret(From, Cmd, false);
	_ ->
	    ok
    end,
    {noreply, State};
handle_call_execute(_Called, Cmd, From, State) ->
    yate:ret(From, Cmd, false),
    {noreply, State}.


handle_call_route("99991009", Cmd, From, State) ->
    yate:ret(From, Cmd, true, "erl/yate_demo_call/start"),
    {noreply, State};
handle_call_route("clock", Cmd, From, State) ->
    yate:ret(From, Cmd, true, "erl/yate_clock/start"),
    {noreply, State};
handle_call_route("mikael", Cmd, From, State) ->
    error_logger:info_msg("Route mikael~n"),
    yate:ret(From, Cmd, true, "sip/sip:1002@mulder"),
    {noreply, State};
handle_call_route(Called, Cmd, From, State) ->
    yate:ret(From, Cmd, false),
    error_logger:error_msg("Unhandled call.route to: ~p~n", [Called]),
    {noreply, State}.
