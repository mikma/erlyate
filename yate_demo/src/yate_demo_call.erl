%%%
%%% @doc       Demo call
%%% @author    Mikael Magnusson <mikma@users.sourceforge.net>
%%% @copyright 2006 Mikael Magnusson
%%%
-module(yate_demo_call).

-behaviour(gen_fsm).

%% api
-export([start_link/5, start_link/4]).

%% gen_fsm
-export([code_change/4, handle_event/3, handle_info/3, handle_sync_event/4,
	 init/1, terminate/3, route/2]).

-record(sstate, {client, handle, id, call}).

-include("yate.hrl").

-define(TIMEOUT_WAIT_EXEC, 10000). %% 10s

start_link(Client, Cmd, From, Args) ->
    Id = yate_command:fetch_key(id, Cmd),
    start_link(Client, Id, Cmd, From, Args).

%%--------------------------------------------------------------------
%% @spec start_link() -> Result
%%           Result = {ok, Pid} | {error, Reason}
%% @doc Start demo server
%% @end
%%--------------------------------------------------------------------
start_link(Client, Id, Cmd, From, Args) ->
    error_logger:info_msg("yate_demo_call start_link~n"),
    gen_fsm:start_link(yate_demo_call, [Client, Id, Cmd, From, Args], []).

%% gen_fsm
init([Client, Id, ExecCmd, From, _Args]) ->
    error_logger:info_msg("~p ~p Init call ~p~n", [?MODULE, self(), Id]),
    {ok, Call} = yate_call_reg:get_call(Client, Id, ExecCmd),
    {ok, Handle} = yate:open(Client),

    NewCmd = yate_command:append_keys([
				   {callto, "dumb/"},
				   {autoring, true}
				  ],
				  ExecCmd),
    yate:ret(From, NewCmd, false),

    {ok, route, #sstate{client=Client, handle=Handle, id=Id, call=Call}, ?TIMEOUT_WAIT_EXEC}.

%% Async
%% stateName(Event, StateData) ->
%%     {next_state, NextStateName, NewStateData, Timeout}.

route(timeout, StateData) ->
    {stop, error, StateData}.

handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

%% Sync
%% stateName(Event, From, StateData) ->
%%     {next_state, NextStateName, NewStateData, Timeout},
%%     {reply, Reply, NextStateName, NewStateData, Timeout}.

handle_sync_event(_Event, _From, StateName, StateData) ->
%    {next_state, NextStateName, NewStateData, Timeout}.
    {reply, {error, unknown_event}, StateName, StateData}.


handle_info({yate_notify, Tag}, StateName, StateData) ->
    error_logger:info_msg("Notify ~p~n", [Tag]),
    ok = yate_call:drop(StateData#sstate.call, noconn),
    {next_state, StateName, StateData};

handle_info({yate_call, execute, _From}, _StateName, StateData) ->
    error_logger:info_msg("Call execute ~p. answer~n", [?MODULE]),

    ok = yate_call:answer(StateData#sstate.call),
    ok = play_wave(StateData),
    {next_state, execute, StateData};

handle_info({yate_call, disconnected, _Cmd, _From}, _StateName, StateData) ->
    {next_state, disconnected, StateData};

handle_info({yate_call, hangup, _From}, _StateName, StateData) ->
    error_logger:info_msg("Call hangup ~p~n", [self()]),
    {stop, normal, StateData};

handle_info(Info, StateName, StateData) ->
    error_logger:error_msg("Unsupported info: ~p~n", [Info]),
    {next_state, StateName, StateData}.


terminate(_Reason, _StateName, StateData) ->
    Handle = StateData#sstate.handle,
    yate:close(Handle),
    terminate.

code_change(_OldVsn, StateName, StateData, _Extra)  ->
    {ok, StateName, StateData}.


record_wave(StateData) ->
    Call = StateData#sstate.call,
    ok = yate_call:record_wave(Call, StateData#sstate.id,
			       "/tmp/record.alaw", 1000000).
play_wave(StateData) ->
    Call = StateData#sstate.call,
    ok = yate_call:play_wave(Call, StateData#sstate.id,
			"/usr/share/asterisk/sounds/digits/0.gsm").
