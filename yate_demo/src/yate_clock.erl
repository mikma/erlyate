%%%
%%% @doc       Demo call
%%% @author    Mikael Magnusson <mikma@users.sourceforge.net>
%%% @copyright 2006 Mikael Magnusson
%%%
-module(yate_clock).

-behaviour(gen_server).

%% api
-export([start_link/4, start_link/5, start/4]).

%% gen_fsm
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2,
	 init/1, terminate/2, route/2]).

%% Debug
-export([wave_year/1, wave_number/1, wave_tens/1, wave_ones/1]).

-record(sstate, {handle, id, waves, call}).

-include("yate.hrl").

-define(TIMEOUT_WAIT_EXEC, 10000). %% 10s

start(Client, Cmd, From, Args) ->
    Id = yate_command:fetch_key(id, Cmd),
    start(Client, Id, Cmd, From, Args).

start_link(Client, Cmd, From, Args) ->
    Id = yate_command:fetch_key(id, Cmd),
    start_link(Client, Id, Cmd, From, Args).

start(Client, Id, Cmd, From, Args) ->
    gen_server:start(yate_clock, [Client, Id, Cmd, From, Args], []).

%%--------------------------------------------------------------------
%% @spec start_link() -> Result
%%           Result = {ok, Pid} | {error, Reason}
%% @doc Start demo server
%% @end
%%--------------------------------------------------------------------
start_link(Client, Id, Cmd, From, Args) ->
    gen_server:start_link(yate_clock, [Client, Id, Cmd, From, Args], []).

%% gen_server
init([Client, Id, ExecCmd, From, _Args]) ->
    error_logger:info_msg("yate_clock start_link ~p~n", [self()]),
    {{Year, Month, Day}, {_Hour, _Min, _Secs}} = erlang:localtime(),
    Waves = wave_month(Month) ++ wave_day(Day) ++ wave_year(Year),
    error_logger:info_msg("Waves ~p~n", [Waves]),
    error_logger:info_msg("Init clock ~p~n", [Id]),

    {ok, Call} = yate_call_reg:get_call(Client, Id, ExecCmd),
    {ok, Handle} = yate:open(Client),

    NewCmd = yate_command:append_keys([
				   {callto, "dumb/"},
				   {autoring, false}
				  ],
				  ExecCmd),
    yate:ret(From, NewCmd, false),

    {ok, #sstate{handle=Handle, id=Id, waves=Waves, call=Call}, ?TIMEOUT_WAIT_EXEC}.

route(timeout, State) ->
    {stop, error, State}.

handle_cast(_Request, _State) ->
    exit(unhandled_cast).

handle_call(_Request, _From, _State) ->
    exit(unhandled_call).


handle_info(answer, State) ->
    ok = yate_call:answer(State#sstate.call),
    {noreply, State, 20};

handle_info({yate_call, execute, _From}, State) ->
    ok = yate_call:ringing(State#sstate.call),
    timer:send_after(1000, answer),
    {noreply, State};
handle_info({yate_call, hangup, _From}, State) ->
    {stop, normal, State};
handle_info({yate_call, disconnected, _From}, State) ->
    {noreply, State};
handle_info({yate_notify, Tag}, State=#sstate{id=Tag}) ->
    handle_notify(State#sstate.waves, State);
handle_info(timeout, State) ->
    handle_timeout(State#sstate.waves, State);
handle_info(Info, State) ->
    error_logger:error_msg("Unsupported info: ~p~n", [Info]),
    {noreply, State}.


terminate(_Reason, State) ->
    Handle = State#sstate.handle,
    yate:close(Handle),
    terminate.

code_change(_OldVsn, State, _Extra)  ->
    {ok, State}.


handle_notify([], State) ->
    ok = yate_call:drop(State#sstate.call),
    {noreply, State};
handle_notify([_Wave_file | _R], State) ->
    {noreply, State, 10}.

handle_timeout([], State) ->
    ok = yate_call:drop(State#sstate.call),
    {stop, normal, State};
handle_timeout([Wave_file | R], State) ->
    [Wave_file | R] = State#sstate.waves,
    ok = play_wave(State, Wave_file),
    {noreply, State#sstate{waves=R}}.

play_wave(State, Wave_file) ->
    yate_call:play_wave(State#sstate.call, State#sstate.id, Wave_file).

wave_month(Month) ->
    [[asterisk_sound_path() ++ "mon-",
     integer_to_list(Month - 1),
     ".gsm"]].

wave_day(Day) ->
    if
	Day == 31 ->
	    [[asterisk_sound_path(), "h-30.gsm"],
	     [asterisk_sound_path(), "h-1.gsm"]];
	Day == 30  ->
	    [[asterisk_sound_path(), "h-30.gsm"]];
	Day > 20 ->
	    [[asterisk_sound_path(), "h-20.gsm"],
	     [asterisk_sound_path(), "h-",
	      integer_to_list(Day - 20),
	      ".gsm"]];
	Day == 20 ->
	    [[asterisk_sound_path(), "h-20.gsm"]];
	true  ->
	    [[asterisk_sound_path(), "h-",
	     integer_to_list(Day),
	     ".gsm"]]
    end.

wave_year(Year) ->
    Hundred = trunc(Year / 100),
    Rest = Year - Hundred * 100,
    [wave_number(Hundred), [asterisk_sound_path(), "hundred.gsm"], wave_number(Rest)].

wave_number(Number) ->
    Tens = trunc(Number / 10),
    Ones = Number - Tens * 10,
    wave_tens(Tens) ++ wave_ones(Ones).

wave_tens(Tens) ->
    if
	Tens > 0 ->
	    [[asterisk_sound_path(),
	      integer_to_list(Tens), "0.gsm"]];
	true ->
	    []
    end.
    
wave_ones(Ones) ->
    if
	Ones > 0 ->
	    [[asterisk_sound_path(),
	      integer_to_list(Ones), ".gsm"]];
	true ->
	    []
    end.

asterisk_sound_path() ->
    "/usr/share/asterisk/sounds/digits/".

