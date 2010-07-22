%%%
%%% @doc       Yate call
%%% @author    Mikael Magnusson <mikma@users.sourceforge.net>
%%% @copyright 2006-2008 Mikael Magnusson
%%%
-module(yate_call).

-behaviour(gen_server).

-include("yate.hrl").

%% api
-export([
	 start_link/1,
	 start_link/2,
	 start_link/3,
	 execute/2,
	 answer/1, drop/2, drop/1,
	 play_wave/3,
	 play_tone/2,
	 record_wave/4,
	 start_rtp/2,
	 start_rtp/3,
	 ringing/1, progress/1, send_dtmf/2, stop/1]).

%% gen_server callbacks
-export([init/1,
	 code_change/3,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2]).

-record(state, {
	  client,
	  handle,
	  parent,
	  id,
	  peerid,
	  rtpid,
	  localip,
	  status				% incoming or outgoing
	 }).

%%--------------------------------------------------------------------
%% @spec start_link(Client) -> Result
%%           Client = pid()
%% @doc Start an outgoing call, setting caller as owner
%% @end
%%--------------------------------------------------------------------
start_link(Client) when is_pid(Client) ->
    start_link(Client, self()).

%%--------------------------------------------------------------------
%% @spec start_link(Client, Owner) -> Result
%%           Client = pid()
%%           Owner = pid()
%% @doc Start an outgoing call, with specified owner.
%% @end
%%--------------------------------------------------------------------
start_link(Client, Owner) when is_pid(Client), is_pid(Owner) ->
    gen_server:start_link(?MODULE, [Client, Owner], []);

%%--------------------------------------------------------------------
%% @spec start_link(Client, Cmd) -> Result
%%           Client = pid()
%% @doc Start an incoming call, setting caller as owner.
%% @end
%%--------------------------------------------------------------------
start_link(Client, Cmd) ->
    start_link(Client, Cmd, self()).

%%--------------------------------------------------------------------
%% @spec start_link(Client, Cmd, Owner) -> Result
%%           Client = pid()
%%           Owner = pid()
%% @doc Start an incoming call, with specified owner.
%% @end
%%--------------------------------------------------------------------
start_link(Client, Cmd, Owner) ->
    gen_server:start_link(?MODULE, [Client, Cmd, Owner], []).

%%--------------------------------------------------------------------
%% @spec execute(Call, Keys) -> Result
%%           Keys = [{Key, Value}]
%%           Key = atom()
%% @doc Execute an outbound call.
%% @end
%%--------------------------------------------------------------------
execute(Call, Keys) ->
    gen_server:call(Call, {execute, Keys}).

%%--------------------------------------------------------------------
%% @spec answer(Call) -> Result
%% @doc Answer an incoming call.
%% @end
%%--------------------------------------------------------------------
answer(Call) ->
    gen_server:call(Call, answer).

%%--------------------------------------------------------------------
%% @spec drop(Call) -> Result
%% @doc Hangup a call.
%% @end
%%--------------------------------------------------------------------
drop(Call) ->
    drop(Call, "hangup").

%%--------------------------------------------------------------------
%% @spec drop(Call, Reason) -> Result
%%           Reason = string()
%% @doc Hangup a call, giving a reason.
%% @end
%%--------------------------------------------------------------------
drop(Call, Reason) ->
    gen_server:call(Call, {drop, Reason}).

%%--------------------------------------------------------------------
%% @spec play_wave(Call, Notify, WaveFile) -> Result
%%           WaveFile = string()
%% @doc Play a sound file. 
%% @end
%%--------------------------------------------------------------------
play_wave(Call, Notify, WaveFile) ->
    error_logger:info_msg("play_wave ~p ~p ~p~n", [?MODULE, self(), Notify]),
    gen_server:call(Call, {play_wave, Notify, WaveFile, self()}).

%%--------------------------------------------------------------------
%% @spec play_tone(Call, Tone) -> Result
%% @doc Play a tone. 
%% @end
%%--------------------------------------------------------------------
play_tone(Call, Tone) ->
    error_logger:info_msg("play_tone ~p ~p ~p~n", [?MODULE, self(), Tone]),
    gen_server:call(Call, {play_tone, Tone}).

%%--------------------------------------------------------------------
%% @spec record_wave(Call, Notify, WaveFile, MaxLen) -> Result
%% @doc Play a sound file.
%% @end
%%--------------------------------------------------------------------
record_wave(Call, Notify, WaveFile, MaxLen) ->
    gen_server:call(Call, {record_wave, Notify, WaveFile, MaxLen, self()}).

%%--------------------------------------------------------------------
%% @spec start_rtp(Call, Remote_address, Remote_port) -> Result
%% @doc Start rtp transmission.
%% @end
%%--------------------------------------------------------------------
start_rtp(Call, Remote_address, Remote_port) ->
    error_logger:info_msg("~p: start_rtp~n", [?MODULE]),
    gen_server:call(Call, {start_rtp, Remote_address, Remote_port}).

%%--------------------------------------------------------------------
%% @spec start_rtp(Call, Remote_address) -> Result
%% @doc Start rtp transmission.
%% @end
%%--------------------------------------------------------------------
start_rtp(Call, Remote_address) ->
    error_logger:info_msg("~p: start_rtp~n", [?MODULE]),
    gen_server:call(Call, {start_rtp, Remote_address}).

%%--------------------------------------------------------------------
%% @spec send_dtmf(Call, Dtmf) -> Result
%% @doc Send dtmf key.
%% @end
%%--------------------------------------------------------------------
send_dtmf(Call, Dtmf) ->
    gen_server:call(Call, {send_dtmf, Dtmf}).

%%--------------------------------------------------------------------
%% @spec ringing(Call) -> Result
%% @doc Send ringing on incoming call.
%% @end
%%--------------------------------------------------------------------
ringing(Call) ->
    gen_server:call(Call, ringing).

%%--------------------------------------------------------------------
%% @spec progress(Call) -> Result
%% @doc Send session progress on incoming call.
%% @end
%%--------------------------------------------------------------------
progress(Call) ->
    gen_server:call(Call, progress).

stop(Call) ->
    gen_server:cast(Call, stop).

%%
%% gen_server callbacks
%%
%% @private
init([Client, Parent]) ->
    init_common(outgoing, Client, [], Parent);

init([Client, Cmd, Parent]) when is_record(Cmd, command) ->
    init_common(incoming, Client, [Cmd], Parent).

init_common(Status, Client, Args, Parent) ->
    error_logger:info_msg("~p: ~p ~p~n", [?MODULE, self(), Status]),
    process_flag(trap_exit, true),
    link(Parent),
    {ok, Handle} = yate:open(Client),
    State0 = #state{client=Client,parent=Parent,handle=Handle},
    setup(Status, Args, State0).


setup(incoming, [Cmd], State) ->
    Id = yate_command:fetch_key(id, Cmd),
    Handle = State#state.handle,
    error_logger:info_msg("~p: ~p watch call.execute id ~p~n", [?MODULE, self(), Id]),
    ok = yate:watch(Handle, call.execute,
		    fun(Cmd1) ->
			    case yate_command:find_key(module, Cmd1) of
				{ok, _Driver} ->
				    Id == yate_command:fetch_key(id, Cmd1);
				_ ->
				    false
			    end
		    end),
    {ok, State#state{id=Id,status=incoming}};

setup(outgoing, [], State) ->
    {ok, State}.


fetch_auto_keys(Cmd) ->	    
    Autokeys = [answered, ringing, progress],
    case fetch_auto_keys(Cmd, Autokeys, []) of
	{ok, noauto} ->
	    case yate_command:find_key(targetid, Cmd) of
		error ->
		    {ok, answered};
		_ ->
		    {ok, dialog}
	    end;
	{ok, Auto} ->
	    Auto
    end.

fetch_auto_keys(_Cmd, [], _Res) ->
    {ok, noauto};
fetch_auto_keys(Cmd, [Key|R], Res) ->
    case yate_command:find_key(Key, Cmd) of
	{ok, "true"} ->
	    {ok, Key};
	_ ->
	    fetch_auto_keys(Cmd, R, Res)
    end.


%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @private
handle_call({execute, Keys}, _From, State) ->
    Handle = State#state.handle,
    Parent = State#state.parent,
    Id = State#state.id,
    %% TODO async msg
    Keys1 = [{message, call.execute},
             {id, Id}
             |Keys],
    {ok, RetValue, RetCmd} = yate:send_msg(Handle, chan.masquerade, Keys1),
    case RetValue of
	false ->
	    %% TODO return false
%% 	    Parent ! {yate_call, notfound, self()},
%% 	    {stop, normal, State};
	    %% TODO change to error state?
	    gen_server:cast(self(), stop),
	    {reply, {error, {noroute, RetCmd}}, State};
	ok ->
	    Auto = fetch_auto_keys(RetCmd),
	    Id1 = yate_command:fetch_key(id, RetCmd),
	    Peerid1 = yate_command:fetch_key(peerid, RetCmd),
	    State1 = State#state{id=Id1,peerid=Peerid1,status=outgoing},
	    ok = setup_watches(State1),
	    Parent ! {yate_call, Auto, RetCmd, self()},
	    {reply, ok, State1}
    end;


%% @private
handle_call(answer, _From, State) ->
    Id = State#state.id,
    Peerid = State#state.peerid,
    Handle = State#state.handle,
    {ok, _RetValue, _RetCmd} =
	yate:send_msg(Handle, call.answered,
		      [
		       {id, Peerid},
                       {targetid, Id},
		       {module, "erlang"}
		      ]),
    {reply, ok, State};

handle_call({drop, Reason}, _From, State) ->
    {ok, State1} = handle_drop(Reason, State),
    {reply, ok, State1};

handle_call({play_wave, Notify, WaveFile, Pid}, _From, State) ->
    Id = State#state.id,
    Peerid = State#state.peerid,
    Handle = State#state.handle,
    {ok, NotifyPid} = yate_notify:start_link(State#state.client, Notify, Pid),
    {ok, NotifyId} = yate_notify:get_id(NotifyPid),
    {ok, _RetValue, _RetCmd} =
	yate:send_msg(Handle, chan.masquerade,
		      [{message, chan.attach},
		       {id, Id},
		       {notify, NotifyId},
		       {override, ["wave/play/", WaveFile]}
		      ]),
    {reply, ok, State};

handle_call({play_tone, Tone}, _From, State) ->
    Id = State#state.peerid,
    Handle = State#state.handle,
    {ok, _RetValue, _RetCmd} =
	yate:send_msg(Handle, chan.masquerade,
		      [{message, chan.attach},
		       {id, Id},
		       {source, "tone/" ++ Tone}]),
    {reply, ok, State};

handle_call({record_wave, Notify, WaveFile, MaxLen, Pid}, _From, State) ->
    Id = State#state.peerid,
    Handle = State#state.handle,
    {ok, NotifyPid} = yate_notify:start_link(State#state.client, Notify, Pid, 2000),
    {ok, NotifyId} = yate_notify:get_id(NotifyPid),

    ExtraParams =
	if
	    MaxLen /= undefined ->
		[{maxlen, MaxLen}];
	    true ->
		[]
	end,

    {ok, _RetValue, _RetCmd} =
	yate:send_msg(Handle, chan.masquerade,
		      [{message, chan.attach},
		       {id, Id},
		       {notify, NotifyId},
		       {consumer, ["wave/record/", WaveFile]}
		      ] ++ ExtraParams),

%%     PeerId = State#state.peerid,
%%     {ok, _RetValue1, _RetCmd1} =
%% 	yate:send_msg(Handle, chan.masquerade,
%% 		      [{message, call.execute},
%%  		       {id, PeerId},
%% %% 		       {lonely, true},
%% %% 		       {voice, false},
%% %% 		       {echo, false},
%% %% 		       {smart, true},
%% 		       {callto, "tone/ring"}
%% 		      ] ++ ExtraParams),
    {reply, ok, State};

handle_call({start_rtp, Remote_address, Remote_port}, _From, State) ->
    Id = State#state.peerid,
    Handle = State#state.handle,
    Format = alaw,

    {ok, RetValue, RetCmd} =
	yate:send_msg(Handle, chan.masquerade,
		      [
		       {message, chan.rtp},
		       {id, Id},
		       {transport, "RTP/AVP"},
		       {direction, bidir},
		       {media, audio},
		       {remoteip, Remote_address},
		       {remoteport, Remote_port},
		       {format, Format}
		      ]),
    
    {Reply, State1} =
	case RetValue of
	    true ->
		Localip =
		    case yate_command:find_key(localip, RetCmd) of
			{ok, Localip1} ->
			    Localip1;
			error ->
			    State#state.localip
		    end,

		Localport =
		    case yate_command:find_key(localport, RetCmd) of
			{ok, Localport1} ->
			    list_to_integer(Localport1);
			error ->
			    undefined
		    end,
		Rtpid =
		    case yate_command:find_key(rtpid, RetCmd) of
			{ok, Rtpid1} ->
			    Rtpid1;
			error ->
			    undefined
		    end,
		State2 = State#state{rtpid=Rtpid, localip=Localip},
		{{ok, Localip, Localport}, State2};

	    false ->
		{{error, yate_error}, State}
	end,
    {reply, Reply, State1};

handle_call({start_rtp, Remote_address}, _From, State) ->
    Id = State#state.peerid,
    Handle = State#state.handle,
    Format = alaw,

    {ok, true, RetCmd} =
	yate:send_msg(Handle, chan.masquerade,
		      [
		       {message, chan.rtp},
		       {transport, "RTP/AVP"},
		       {media, "audio"},
		       {direction, "receive"},
		       {id, Id},
		       {format, Format},
		       {remoteip, Remote_address}
		      ]),

    Localip =
	case yate_command:find_key(localip, RetCmd) of
	    {ok, Localip1} ->
		Localip1;
	    error ->
		State#state.localip
	end,
    Localport = list_to_integer(yate_command:fetch_key(localport, RetCmd)),

    {reply, {ok, Localip, Localport}, State#state{localip=Localip}};

handle_call(ringing, _From, State) ->
    Id = State#state.id,
    Peerid = State#state.peerid,
    Handle = State#state.handle,
    {ok, _RetValue, _RetCmd} =
	yate:send_msg(Handle, call.ringing,
		      [
 		       {id, Peerid},
                       {targetid, Id},
		       {module, "erlang"}
		      ]),
    
    {reply, ok, State};

handle_call(progress, _From, State) ->
    Id = State#state.id,
    Peerid = State#state.peerid,
    Handle = State#state.handle,
    {ok, _RetValue, _RetCmd} =
	yate:send_msg(Handle, chan.progress,
		      [
 		       {id, Peerid},
                       {targeid, Id},
		       {module, "erlang"},
		       %% FIXME enable only if available.
		       {earlymedia, true},
 		       {media, yes}
		      ]),
    
    {reply, ok, State};

handle_call({send_dtmf, Dtmf}, _From, State) ->
    Id = State#state.peerid,
    Rtpid = State#state.rtpid,
    Handle = State#state.handle,
    ok =
	yate:queue_msg(Handle, chan.masquerade,
		      [
		       {message, chan.dtmf},
 		       {id, Id},
		       {targetid, Rtpid},
		       {text, Dtmf}
		      ]),
    {reply, ok, State};

handle_call(Request, _From, State) ->
    error_logger:error_msg("Unhandled call in ~p: ~p~n", [?MODULE, Request]),
    {reply, ok, State}.


%% @private
handle_cast(stop, State) ->
    error_logger:error_msg("~p:stop received ~p~n", [?MODULE, self()]),
    {stop, normal, State};
handle_cast(Request, State) ->
    error_logger:error_msg("Unhandled cast in ~p: ~p~n", [?MODULE, Request]),
    {noreply, State}.


%% @private
handle_info({yate_req, Cmd, From}, State) ->
    error_logger:error_msg("~p:received cmd ~p~n", [?MODULE, self()]),
    handle_command(Cmd#command.type, req, Cmd, From, State);
handle_info({yate_ans, Cmd, From}, State) ->
    error_logger:error_msg("~p:received cmd ~p~n", [?MODULE, self()]),
    handle_command(Cmd#command.type, ans, Cmd, From, State);
handle_info({'EXIT', Pid, Reason}, State=#state{parent=Pid}) ->
    error_logger:error_msg("~p:~p ~p terminated, do drop ~p~n", [?MODULE, self(), Pid, Reason]),
    {ok, State1} = handle_drop("Error", State),
    {noreply, State1};
handle_info({'EXIT', _Pid, normal}, State) ->
    %% Ignore normal exit
    {noreply, State};
handle_info({'EXIT', _Pid, Reason}, State) ->
    error_logger:error_msg("~p:EXIT received ~p~n", [?MODULE, self()]),
    {stop, Reason, State};

handle_info(Info, State) ->
    error_logger:error_msg("Unhandled info in ~p: ~p~n", [?MODULE, Info]),
    {noreply, State}.


%% @private
terminate(Reason, State) ->
    error_logger:error_msg("~p: Terminating ~p~n", [?MODULE, Reason]),
    Handle = State#state.handle,
    yate:close(Handle),
    terminated.


handle_command(message, Dir, Cmd, From, State) ->
    Name = (Cmd#command.header)#message.name,
    error_logger:error_msg("~p:received ~p ~p ~p~n", [?MODULE, self(), Name, Dir]),
    handle_message(Name, Dir, Cmd, From, State).


handle_message(call.execute, ans, Cmd, _From, State) ->
    Peerid = yate_command:fetch_key(peerid, Cmd),
    State1 = State#state{peerid=Peerid},
    ok = setup_watches(State1),
    Parent = State#state.parent,
    Parent ! {yate_call, execute, self()},
    {noreply, State1};
handle_message(call.ringing, ans, Cmd, _From, State) ->
    Parent = State#state.parent,
    Parent ! {yate_call, ringing, Cmd, self()},
    {noreply, State};
handle_message(call.progress, ans, Cmd, _From, State) ->
    Parent = State#state.parent,
    Parent ! {yate_call, progress, Cmd, self()},
    {noreply, State};
handle_message(call.answered, ans, Cmd, _From, State) ->
    Parent = State#state.parent,
    Parent ! {yate_call, answered, Cmd, self()},
    {noreply, State};
handle_message(chan.disconnected, ans, Cmd, _From, State) ->
    Parent = State#state.parent,
    Parent ! {yate_call, disconnected, Cmd, self()},
    {noreply, State};
handle_message(chan.dtmf, req, Cmd, From, State) ->
    Parent = State#state.parent,
    Parent ! {yate_call, dtmf, Cmd, self()},
    ok = yate:ret(From, Cmd, true),
    {noreply, State};
handle_message(chan.hangup, ans, _Cmd, _From, State) ->
    Parent = State#state.parent,
    Parent ! {yate_call, hangup, self()},
    error_logger:error_msg("~p:hangup received ~p~n", [?MODULE, self()]),
    {stop, normal, State};
handle_message(Message, Dir, _Cmd, _From, State) ->
    error_logger:error_msg("~p:Unhandled message ~p ~p ~p~n", [?MODULE, self(), Message, Dir]),
    {noreply, State}.


handle_drop(Reason, State) ->
    Id = State#state.id,
    Peerid = State#state.peerid,
    Handle = State#state.handle,
    {ok, _RetValue, _RetCmd} =
	yate:send_msg(Handle, call.drop,
		      [
		       {id, Peerid},
                       {targetid, Id},
		       {reason, Reason},
		       {module, "erlang"}
		      ]),
    {ok, State}.


setup_watches(State) ->
    Handle = State#state.handle,
    Id = State#state.id,
    Peerid = State#state.peerid,
    ok = yate:watch(Handle, chan.disconnected,
		    fun(Cmd) ->
			    Id == yate_command:fetch_key(id, Cmd)
		    end),
    ok = yate:watch(Handle, call.ringing,
		    fun(Cmd) ->
 			    Peerid == yate_command:fetch_key(targetid, Cmd)
		    end),
    ok = yate:watch(Handle, chan.hangup,
		    fun(Cmd) ->
			    Id == yate_command:fetch_key(id, Cmd)
		    end),
    ok = yate:watch(Handle, call.progress,
		    fun(Cmd) ->
			    Peerid == yate_command:fetch_key(targetid, Cmd)
		    end),
    ok = yate:watch(Handle, call.answered,
		    fun(Cmd) ->
			    Peerid == yate_command:fetch_key(targetid, Cmd)
		    end),
    ok = yate:install(Handle, chan.dtmf,
		      fun(Cmd) ->
			      Peerid == yate_command:fetch_key(targetid, Cmd)
		      end),
    ok.
