%%
%% Based on sipclient.erl from Yxa
%%

%%
%% States:
%% incoming - SIP to Yate
%% outgoing - Yate to SIP
%% up       - Call up
%% bye_sent - SIP BYE sent
%%

-module(sipclient).

-behaviour(gen_fsm).


%% api
-export([start_link/3, stop/0, call/4]).

-export([make/0]).

%% gen_fsm callbacks
-export([init/1,
	 code_change/4,
	 handle_event/3,
	 handle_sync_event/4,
	 handle_info/3,
	 terminate/3]).

-record(state, {
		invite,				% INVITE request
		sip_call,			% SIP call (Pid)
		client,				% Yate client
		handle,				% Yate handle
		call,				% Yate call
		address,			% Remote RTP address
		port,				% Remote RTP port
		contact,			% Local contact
		sdp_body			% Local sdp_body
	       }).

-define(SERVER, ?MODULE).


%% yxa_app callbacks
-export([init/0, request/3, response/3]).

-include("siprecords.hrl").
-include("sipsocket.hrl").
-include("yate.hrl").
-include("sdp.hrl").

-define(DEFAULT_TIMEOUT, 50).
-define(HOST, "localhost").
-define(PORT, 15062).

init() ->
    Server = {ysip_srv, {ysip_srv, start_link, [?HOST, ?PORT]},
	      permanent, 2000, worker, [ysip_srv]},
    Callregister = {callregister, {callregister, start_link, []},
		    permanent, 2000, worker, [callregister]},
    Tables = [],
    [Tables, stateful, {append, [Server, Callregister]}].

request(#request{method="OPTIONS"}=Request, Origin, LogStr) when is_record(Origin, siporigin) ->
    logger:log(normal, "sipclient: Options ~s", [LogStr]),
    siphelper:send_response(Request, 200, "Ok");
request(#request{method="INVITE"}=Request, Origin, LogStr) when is_record(Origin, siporigin) ->
    ysip_srv:invite(Request, LogStr);
request(_Request, _Origin, LogStr) ->
    logger:log(normal, "sipclient: Request ~s", [LogStr]),
    ok.


response(Response, Origin, LogStr) when is_record(Response, response), is_record
(Origin, siporigin) ->        
    {Status, Reason} = {Response#response.status, Response#response.reason},
    if
	Status >= 200, Status =< 299 ->
	    case sipdialog:get_dialog_controller(Response) of
		{ok, Dc_pid} ->
		    logger:log(normal, "sipcall: Response to ~s: '~p ~s', no matching transaction, matching dialog ~p - dropping", [LogStr, Status, Reason, Dc_pid]);
		_ ->
%% 		    logger:log(normal, "sipclient: Response to ~s: '~p ~s', no matching transaction, no matching dialog - dropping X", [LogStr, Status, Reason]),
		    sipcall:response(Response, Origin, LogStr)
	    end;
	true ->
            logger:log(normal, "sipcall: Response to ~s: '~p ~s', no matching transaction - dropping",
		   [LogStr, Status, Reason])
    end,
    ok.

%%
%% outgoing yate call
%%
call(Client, Cmd, From, Args) when is_record(Cmd, command) ->
    Id = command:fetch_key(id, Cmd),
    start_link(Client, Id, Cmd, From, Args).

start_link(Client, Request, LogStr) ->
    logger:log(normal, "sipclient: start_link ~p~n", [self()]),
    gen_fsm:start_link(?MODULE, [Client, Request, LogStr, self()], []).


start_link(Client, Id, Cmd, From, Args) ->
    logger:log(normal, "sipclient: start_link ~p~n", [self()]),
    gen_fsm:start_link(?MODULE, [Client, Id, Cmd, From, Args], []).


stop() ->
    error.

%%
%% gen_fsm callbacks
%%

%% sipcall
init([]) ->
    {ok, undefined};

%% Incoming SIP call
init([Client, Request, LogStr, OldPid]) ->
    init2(Client, Request, LogStr, OldPid);

init([Client, _Id, Cmd, From, [SipUri]]) ->
    {ok, Call} = yate_call:start_link(Client, Cmd),
    {ok, Handle} = yate:open(Client),

    NewCmd = command:append_keys([
				  {callto, "dumb/"},
				  {autoring, false}
				 ],
				 Cmd),
    yate:ret(From, NewCmd, false),

    Caller = case command:find_key(caller, Cmd) of
		 {ok, Caller1} ->
		     Caller1;
		 error ->
		     "anonymous"
	     end,
    CallerName = case command:find_key(callername, Cmd) of
		     {ok, []} ->
			 none;
		     {ok, CallerName1} ->
			 CallerName1;
		     error ->
			 none
		 end,

    CallerUri = "sip:" ++ Caller ++ "@192.168.0.2:5080",

    FromHdr = #contact{display_name = CallerName,
		       urlstr = CallerUri,
		       contact_param = contact_param:to_norm([])
		      },
    ToHdr = #contact{display_name = none,
		     urlstr = SipUri,
		     contact_param = contact_param:to_norm([])
		    },
    Contact = CallerUri,

    State = #state{client=Client,handle=Handle,call=Call,contact=Contact},
    Body = <<>>,

    {ok, Request} = sipcall:build_invite(FromHdr, ToHdr, Body),

    State2 = State#state{invite=Request, sdp_body=Body},
    {ok, outgoing, State2}.

%% Incoming SIP call
init2(Client, Request, LogStr, OldPid) ->
    {ok, Handle} = yate:open(Client),
    logger:log(normal, "sipclient: INVITE ~s ~p~n", [LogStr, self()]),
    {ok, Address, Port} = parse_sdp(Request),

    {ok, SipCall} = sipcall:start_link(?MODULE, [], []),
    ok = sipcall:receive_invite(SipCall, Request, OldPid),

    %% TODO handle incoming call, build sipcall Pid
    State = #state{invite=Request, sip_call=SipCall,
		   handle=Handle, address=Address, port=Port,
		   client=Client},
%%     {ok, _TRef} = timer:send_after(20000, timeout),
    execute(State).


adopt_transaction(THandler, FromPid, ToPid) ->
    logger:log(normal, "sipclient: before change_parent ~p~n", [self()]),
    ok = transactionlayer:change_transaction_parent(THandler, FromPid, ToPid),
    logger:log(normal, "sipclient: after change_parent ~p~n", [self()]),
    ok.


parse_sdp(Request) when is_record(Request, request) ->
    parse_sdp(Request#request.body);

parse_sdp(Response) when is_record(Response, response)->
    parse_sdp(Response#response.body);

parse_sdp(Body) when is_binary(Body) ->
    parse_sdp(binary_to_list(Body));

parse_sdp(Body) when is_list(Body) ->
    case sdp:parse(Body) of
	{ok, Sdp} ->
	    [Media|_] = Sdp#sdp.media,
	    Conn = case Media#sdp_media.connection of
		       undefined ->
			   Sdp#sdp.connection;
		       Conn1 ->
			   Conn1
		   end,
	    Address = Conn#sdp_connection.address,
	    Port = Media#sdp_media.port,
	    {ok, Address, Port};
	{error, _Reason} ->
	    error
    end.

execute(State) ->
    Call_to = "dumb/",
    SipCall = State#state.sip_call,
    Request = State#state.invite,
    Uri = Request#request.uri,
    Target = Uri#sipurl.user,
    From = keylist:fetch('from', Request#request.header),
    [FromContact] = contact:parse(From),
    FromUri = sipurl:parse(FromContact#contact.urlstr),
    Caller = FromUri#sipurl.user,
    Caller_name =
	case FromContact#contact.display_name of
	    none ->
		[];
	    Caller_name1 ->
		[{callername, Caller_name1}]
	end,

    case catch yate_call:execute_link(State#state.client,
			       [
				{caller, Caller},
				{callto, Call_to},
				{target, Target}
			       ] ++ Caller_name) of
	{error, {noroute, _Cmd}} ->
	    sipcall:drop(SipCall, 404, "Not Found"),

	    %% FIXME reason, drop sip_call
%% 	    ok = send_response(State, 404, "Not Found"),
	    {stop, normal};
	{ok, Call} ->
	    execute_finish(Call, State)
    end.

execute_finish(Call, State) ->
    %% FIXME send 101 to sip_call
%%     ok = send_response(State, 101, "Dialog Establishment"),
    State1 = State#state{call=Call},
    {ok, State2} = setup(State1),
    {ok, incoming, State2}.


setup(State) ->   
    %% FIXME Contact
    Contact = "<sip:dummy@192.168.0.2:5080>",

%%     {ok, State1b} = startup(State, Id),
    {ok, State#state{contact=Contact}}.


get_sdp_body(State) ->
    case State#state.sdp_body of
	undefined ->
	    {ok, State1} = start_rtp(State),
	    {ok, State1, State1#state.sdp_body};
	_ ->
	    {ok, State, State#state.sdp_body}
    end.

start_rtp(State) ->
    Call = State#state.call,
    Remote_address = State#state.address,
    Remote_port = State#state.port,
    {ok, Localip, Localport} =
	yate_call:start_rtp(Call, Remote_address, Remote_port),

    {ok, Body} = create_sdp_body(Localip, Localport),
    {ok, State#state{sdp_body=Body}}.

start_rtp_receiver(State, Remote_addr) ->
    Call = State#state.call,
    {ok, Localip, Localport} =
	yate_call:start_rtp(Call, Remote_addr),

    {ok, Body} = create_sdp_body(Localip, Localport),
    {ok, State#state{sdp_body=Body}}.

create_sdp_body(Localip, Localport) ->
    Seconds = integer_to_list(yate_util:seconds()),
    Origin = #sdp_origin{username="-", session_id=Seconds, version=Seconds,
			 network_type='IN', address_type='IP4',
			 address=Localip},
    Connection = #sdp_connection{network_type='IN', address_type='IP4',
				 address = Localip},
    Media = #sdp_media{media=audio, port=Localport, transport="RTP/AVP",
		       fmts=[8], connection=Connection},
    Sdp = #sdp{origin=Origin, session_name="Yxa", media=[Media]},
    Body = list_to_binary(lists:flatten(sdp:print(Sdp))),
    {ok, Body}.

%% TODO move 200ok to separate process and retransmitt
%% send_200ok(State) ->
%%     {ok, State1, Body} = get_sdp_body(State),
%%     ok = send_response(State1, 200, "Ok", [], Body),
%%     {ok, State1}.


code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.


handle_sync_event(Event, _From, StateName, State) ->
    error_logger:error_msg("Unhandled call in ~p: ~p~n", [?MODULE, Event]),
    {reply, ok, StateName, State}.


handle_event(stop, _StateName, State) ->
    {stop, normal, State};
handle_event(Request, StateName, State) ->
    error_logger:error_msg("Unhandled cast in ~p: ~p~n", [?MODULE, Request]),
    {next_state, StateName, State}.


handle_info({yate_call, execute, Call}, outgoing=StateName, State=#state{call=Call, sip_call=undefined}) ->
    error_logger:info_msg("~p: execute ~p~n", [?MODULE, StateName]),
    Remote_addr = "192.168.0.1",
    {ok, State1} = start_rtp_receiver(State, Remote_addr),
    Request = State1#state.invite,
    Body = State1#state.sdp_body,

    Request1 = siprequest:set_request_body(Request, Body),
    {ok, Sip_call} = sipcall:start_link(?MODULE, [], []),
    ok = sipcall:send_invite(Sip_call, Request1),

    State2 = State1#state{sip_call=Sip_call,
			  invite=Request1},
    {next_state, StateName, State2};

handle_info({yate_call, dialog, Call}, incoming=StateName, State=#state{call=Call}) ->
    %% FIXME send 101 to sip_call
%%     ok = send_response(State, 101, "Dialog Establishment"),
    {next_state, StateName, State};

handle_info({yate_call, ringing, Cmd, Call}, incoming=StateName, State=#state{call=Call}) ->
    {ok, State1} = handle_proceeding(180, "Ringing", Cmd, State),
    {next_state, StateName, State1};

handle_info({yate_call, progress, Cmd, Call}, incoming=StateName, State=#state{call=Call}) ->
    {ok, State1} = handle_proceeding(183, "Session Progress", Cmd, State),
    {next_state, StateName, State1};

handle_info({yate_call, answered, _Cmd, Call}, incoming=_StateName, State=#state{call=Call}) ->
    error_logger:info_msg("~p: answer ~n", [?MODULE]),
    %% FIXME send answered to sip_call

    SipCall = State#state.sip_call,
    {ok, State1, Body} = get_sdp_body(State),
    ok = sipcall:answer(SipCall, Body),
    {next_state, up, State1};

handle_info({yate_call, dtmf, Cmd, Call}, StateName, State=#state{call=Call}) ->
    error_logger:info_msg("~p: dtmf ~n", [?MODULE]),

    SipCall = State#state.sip_call,
    Dtmf = command:fetch_key(text, Cmd),
    ok = yate_call:send_dtmf(SipCall, Dtmf),
    {next_state, StateName, State};

handle_info({yate_call, disconnected, Cmd, Call}, StateName, State=#state{call=Call}) ->
    error_logger:info_msg("~p: Call disconnected ~p ~p~n", [?MODULE, Call, StateName]),
    SipCall = State#state.sip_call,
    case command:find_key(reason, Cmd) of
	{ok, YateReason} ->
	    {Status, Reason} = reason_to_sipstatus(list_to_atom(YateReason)),
	    ok = sipcall:drop(SipCall, Status, Reason);
	_ ->
	    ok = sipcall:drop(SipCall)
	end,
    {stop, normal, State};

handle_info({yate_call, hangup, Call}, StateName, State=#state{call=Call}) ->
    error_logger:info_msg("~p: Call hangup ~p ~p~n", [?MODULE, Call, StateName]),
    {next_state, StateName, State};

handle_info(timeout, incoming=StateName, State) ->
    %% FIXME drop sip_call
%%     ok = send_response(State#state.invite, 408, "Request Timeout"),
    ok = yate_call:drop(State#state.call, "Request Timeout"),
    {next_state, StateName, State};


%% new_request "BYE":
%% ok = yate_call:drop(State#state.call, "Normal Clearing"),
%% {stop, NewDialog1};

handle_info({call_drop, SipCall, Response}, _StateName, #state{sip_call=SipCall}=State) when is_record(Response, response) ->
    Call = State#state.call,
    Status = Response#response.status,
    Reason = sipstatus_to_reason(Status),
    ok = yate_call:drop(Call, Reason),
    {stop, normal, State};

handle_info({call_drop, SipCall, Request}, _StateName, #state{sip_call=SipCall}=State) when is_record(Request, request) ->
    %% TODO check reason code
    Call = State#state.call,
%%     Status = Response#response.status,
%%     Reason = sipstatus_to_reason(Status),
    ok = yate_call:drop(Call), %, Reason),
    {stop, normal, State};

handle_info({call_drop, SipCall, ExtraHeaders}, _StateName, #state{sip_call=SipCall}=State) when is_list(ExtraHeaders) ->
    %% TODO check reason code
    Call = State#state.call,
%%     Status = Response#response.status,
%%     Reason = sipstatus_to_reason(Status),
    ok = yate_call:drop(Call), %, Reason),
    {stop, normal, State};


handle_info({call_proceeding, SipCall, Response}, outgoing=StateName, #state{sip_call=SipCall}=State) when is_record(Response, response) ->
    Call = State#state.call,

    %% FIX parse sdp

    case Response#response.status of
	180 ->
	    ok = yate_call:ringing(Call);
	183 ->
	    ok = yate_call:progress(Call)
    end,

    case parse_sdp(Response) of
	{ok, Remote_address, Remote_port} ->
	    {ok, _Localip, _Localport} =
		yate_call:start_rtp(Call, Remote_address, Remote_port);
	error ->
	    ok
    end,

    {next_state, StateName, State};

handle_info({call_redirect, SipCall, Response}, outgoing=StateName, #state{sip_call=SipCall}=State) when is_record(Response, response) ->
    Call = State#state.call,
    ok = yate_call:drop(Call, forbidden),
    {next_state, StateName, State};

handle_info({call_answered, SipCall, Response}, outgoing=_StateName, #state{sip_call=SipCall}=State) when is_record(Response, response) ->
    Call = State#state.call,
    ok = yate_call:answer(Call),

    %% FIXME check reponse body
    {ok, Remote_address, Remote_port} = parse_sdp(Response),

    {ok, _Localip, _Localport} =
	yate_call:start_rtp(Call, Remote_address, Remote_port),
    {next_state, up, State};

handle_info({'EXIT', _Pid, normal}, StateName, State) ->
    %% Ignore normal exit
    {next_state, StateName, State};

handle_info({'EXIT', _Pid, Reason}, _StateName, State) ->
    %% Terminate with error
    {stop, Reason, State};

handle_info(Info, StateName, State) ->
    error_logger:error_msg("~p: Unhandled info in info=~p state_name=~p~n",
			   [?MODULE, Info, StateName]),
    {next_state, StateName, State}.


terminate(Reason, _StateName, _State) ->
    error_logger:error_msg("~p: Terminated ~p~n", [?MODULE, Reason]),
    terminated.


handle_proceeding(Status, Reason, Cmd, State) ->
    SipCall = State#state.sip_call,

    {State1, Body1} =
	case command:find_key(media, Cmd) of
	    {ok, "yes"} ->
		{ok, State2, Body2} = get_sdp_body(State),
		{State2, Body2};

	    _ ->
		{State, <<>>}
	end,

    ok = sipcall:proceeding(SipCall, Status, Reason, Body1),
    {ok, State1}.


sipstatus_to_reason(401) ->
    noauth;
sipstatus_to_reason(403) ->
    forbidden;
sipstatus_to_reason(404) ->
    noroute;
%% sipstatus_to_reason(404) ->
%%     offline;
sipstatus_to_reason(406) ->
    rejected;
sipstatus_to_reason(415) ->
    nomedia;
sipstatus_to_reason(480) ->
    congestion;
sipstatus_to_reason(483) ->
    looping;
sipstatus_to_reason(481) ->
    nocall;
sipstatus_to_reason(484) ->
    incomplete;
sipstatus_to_reason(486) ->
    busy;
sipstatus_to_reason(487) ->
    noanswer;
sipstatus_to_reason(491) ->
    pending;
sipstatus_to_reason(Status) when Status >= 400, Status =< 499 ->
    failure;

sipstatus_to_reason(503) ->
    noconn;
sipstatus_to_reason(Status) when Status >= 500, Status =< 599 ->
    failure;

sipstatus_to_reason(603) ->
    forbidden;
sipstatus_to_reason(606) ->
    rejected;
sipstatus_to_reason(604) ->
    noroute;
sipstatus_to_reason(Status) when Status >= 600, Status =< 699 ->
    busy.

reason_to_sipstatus(Reason) when is_atom(Reason)  ->
    Status = reason_to_sipstatus2(Reason),
    {Status, atom_to_list(Reason)}.

reason_to_sipstatus2(incomplete) ->
    484;
reason_to_sipstatus2(noroute) ->
    404;
reason_to_sipstatus2(noconn) ->
    503;
reason_to_sipstatus2(noauth) ->
    401;
reason_to_sipstatus2(nomedia) ->
    415;
reason_to_sipstatus2(nocall) ->
    481;
reason_to_sipstatus2(busy) ->
    486;
reason_to_sipstatus2(noanswer) ->
    487;
reason_to_sipstatus2(rejected) ->
    406;
reason_to_sipstatus2(forbidden) ->
    403;
reason_to_sipstatus2(offline) ->
    404;
reason_to_sipstatus2(congestion) ->
    480;
reason_to_sipstatus2(failure) ->
    500;
reason_to_sipstatus2(pending) ->
    491;
reason_to_sipstatus2(looping) ->
    483;
reason_to_sipstatus2(Reason) ->
    error_logger:error_msg("~p: Unknown reason code '~p', returning 500~n",
			   [?MODULE, Reason]),
    500.

%% {404, "Not Found"};
%% {486, "Busy Here"};
%% {403, "Forbidden"};
%% {500, "Internal Server Error"}



make() ->
    Modules = [
	        "callregister",
	        "register_server",
	        "register_sup",
		"sdp",
		"sipcall",
		"sipcall_bye",
		"sipclient",
		"sipregister",
	        "siptest",
	        "siphelper",
		"ysip_srv"
	    ],

    Prefix = "../../../src/esipua/",
    Files = lists:map(fun(File) -> Prefix ++ File end, Modules),

    make:files(Files,
	       [load,
		{i, "../../../include"},
		{i, "/usr/lib/yxa/include"},
		{outdir, "../../src/esipua"},
		debug_info]).
