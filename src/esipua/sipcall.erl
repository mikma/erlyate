%%
%% Based on sipclient.erl from Yxa
%%

%%
%% States:
%% incoming - Incoming calls 
%% outgoing - Outgoing calls
%% up       - Call up
%% bye_sent - SIP BYE sent
%%

-module(sipcall).

-behaviour(gen_fsm).
%% -behaviour(yxa_app).


%% api
-export([
	 start_link/3,
	 stop/1,
	 call/3,
	 build_invite/3,
	 send_invite/2
%%call/1
	]).

%% YXA behaviour
-export([
%% 	 init/0,
	 terminate/1,
	 request/3,
	 response/3
	]).

%% behaviour
-export([behaviour_info/1]).

%% gen_fsm callbacks
-export([init/1,
	 code_change/4,
	 handle_event/3,
	 handle_sync_event/4,
	 handle_info/3,
	 terminate/3]).

-record(state, {dialog,				% SIP Dialog
		module,				% Behaviour module
		options,			% Behaviour options
		invite,				% In/out INVITE request
		invite_pid,			% In/out INVITE pid
		invite_branch,			% Outgoing INVITE branch
		invite_cseqno=0,		% Outgoing cseq
		invite_rseqno,			% Incoming rseqno
		invite_pending,
		bye_branch,			% BYE branch id
		bye_pid,			% BYE pid
		auths=[],
		retry_timer,
		contact				% Local contact
	       }).



-include("siprecords.hrl").
-include("sipsocket.hrl").

behaviour_info(callbacks) ->
    [{init, 1}];
behaviour_info(_Other) ->
    undefined.


%%
%% YXA behaviour implementation
%%
%% init() ->
%%     Server = {ysip_srv, {ysip_srv, start_link, [?HOST, ?PORT]},
%% 	      permanent, 2000, worker, [ysip_srv]},
%%     Tables = [],
%%     [Tables, stateful, {append, [Server]}].


terminate(_Mode) ->
    ok.


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
		    logger:log(normal, "sipclient: Response to ~s: '~p ~s', no matching transaction, matching dialog ~p - dropping", [LogStr, Status, Reason, Dc_pid]);
		_ ->
		    logger:log(normal, "sipclient: Response to ~s: '~p ~s', no matching transaction, no matching dialog - dropping", [LogStr, Status, Reason])
	    end;
	true ->
            logger:log(normal, "sipclient: Response to ~s: '~p ~s', no matching transaction - dropping",
		   [LogStr, Status, Reason])
    end,
    ok.

%%
%% outgoing yate call
%%
call(From, To, Body) when is_record(From, contact),
			  is_record(To, contact),
			  is_binary(Body) ->
    start_link(From, To, Body).

%%--------------------------------------------------------------------
%% Function: generate_new_request(Method, State, Contact)
%%           Method = string(), SIP method
%%           State  = state record()
%% Descrip.: Generate a request template using values from the dialog
%%           state in State#state.dialog, or from the INVITE request
%%           created during startup and stored in
%%           State#state.invite_request (note that the INVITE request
%%           is always created, even though it is not always sent).
%% Returns : {ok, Request, NewDialog}
%%--------------------------------------------------------------------

generate_new_request(Method, Dialog, Contact) ->
    ExtraHeaders = [{"Contact",  [Contact]}],
    {ok, Request, Dialog1, _Dst} = sipdialog:generate_new_request(Method, ExtraHeaders, <<>>, Dialog),
    {ok, Request, Dialog1}.

generate_new_request(Method, Dialog, Contact, CSeqNum) ->
    ExtraHeaders = [{"Contact",  [Contact]}],
    {ok, Request, Dialog1, _Dst} = sipdialog:generate_new_request(Method, ExtraHeaders, <<>>, Dialog, CSeqNum),
    {ok, Request, Dialog1}.


%%
%% build_invite
%%
build_invite(From, To, Body) when is_record(From, contact),
				  is_record(To, contact),
				  is_binary(Body) ->
    {ok, Request, _CallId, _FromTag, _CSeqNo} =
	siphelper:start_generate_request("INVITE",
                               From,
                               To,
                               [
				{"Content-Type", ["application/sdp"]},
				{"Require", ["100rel"]}
                               ],
			       Body
                              ),
    {ok, Request}.

send_invite(Pid, Request) when is_pid(Pid), is_record(Request, request) ->
    gen_fsm:send_all_state_event(Pid, {send_invite, Request}).

%% start_link(Request, LogStr) when is_record(Request, request) ->
%%     logger:log(normal, "sipclient: start_link ~p~n", [self()]),
%%     gen_fsm:start_link(?MODULE, [Request, LogStr, self()], []).

start_link(Module, Args, Options) when is_atom(Module) ->
    gen_fsm:start_link(?MODULE, [Module, Args, Options], Options);

start_link(From, To, Body) when is_record(From, contact),
				is_record(To, contact),
				is_binary(Body) ->
    logger:log(normal, "sipclient: start_link ~p~n", [self()]),
    gen_fsm:start_link(?MODULE, [From, To, Body], []).


stop(Pid) ->
    gen_fsm:send_all_state_event(Pid, stop).

%%
%% gen_fsm callbacks
%%
init([Module, Args, Options]) when is_atom(Module) ->
    case Module:init(Args) of
	{ok, _SubState} ->
	    {ok, start, #state{module=Module, options=Options}};
	{stop, Reason} ->
	    {stop, Reason};
	ignore ->
	    ignore
    end;


init([Request, LogStr, OldPid]) when is_record(Request, request) ->
    case transactionlayer:adopt_st_and_get_branchbase(Request) of
	ignore ->
	    {stop, {error, ignore}};
	error ->
	    {stop, error};
	BranchBase ->
	    init2(Request, LogStr, BranchBase, OldPid)
    end;


%% Outgoing call
init([Request]) when is_record(Request, request) ->
    ok.




init2(Request, LogStr, _BranchBase, OldPid) ->
    logger:log(normal, "sipclient: INVITE ~s ~p~n", [LogStr, self()]),
    THandler = transactionlayer:get_handler_for_request(Request),
    ok = transactionlayer:change_transaction_parent(THandler, OldPid, self()),
    Invite_pid = transactionlayer:get_pid_from_handler(THandler),
    State = #state{invite=Request, invite_pid=Invite_pid},
%%     {ok, _TRef} = timer:send_after(20000, timeout),
    execute(State).

execute(State) ->
    catch case a:'TODO'() of
	{error, {noroute}} ->
	    %% FIXME reason
	    ok = send_response(State, 404, "Not Found"),
	    {stop, normal};
	ok ->
	    execute_finish(State)
    end.

execute_finish(State) ->
    ok = send_response(State, 101, "Dialog Establishment"),
    {ok, State2} = setup(State),
    {ok, incoming, State2}.


setup(State) ->   
    Request = State#state.invite,

    %% FIXME Contact
    Contact = "<sip:dummy@192.168.0.7:5080>",
    {ok, Dialog} = create_dialog(Request, Contact),

%%     {ok, State1b} = startup(State, Id),
    {ok, State#state{contact=Contact,dialog=Dialog}}.


create_dialog(Request, Contact) ->
    throw({error, bad}),
    THandler = transactionlayer:get_handler_for_request(Request),
    {ok, ToTag} = transactionlayer:get_my_to_tag(THandler),
    {ok, Dialog} = sipdialog:create_dialog_state_uas(Request, ToTag, Contact),
    ok = sipdialog:register_dialog_controller(Dialog, self()),
    {ok, Dialog}.

%% TODO move 200ok to separate process and retransmitt
%% send_200ok(State) ->
%%     Body = "TODO",
%%     ok = send_response(State, 200, "Ok", [], Body),
%%     {ok, State}.


send_response(State, Status, Reason) when is_record(State, state) ->
    siphelper:send_response(State, Status, Reason, []).

send_response(State, Status, Reason, ExtraHeaders) when is_record(State, state) ->
    siphelper:send_response(State, Status, Reason, ExtraHeaders, <<>>).

send_response(State, Status, Reason, ExtraHeaders, Body) when is_record(State, state) ->
    Request = State#state.invite,
    Contact = State#state.contact,
    siphelper:send_response(Request, Status, Reason, ExtraHeaders, Body, Contact).

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.


handle_sync_event(Event, _From, StateName, State) ->
    error_logger:error_msg("Unhandled call in ~p: ~p~n", [?MODULE, Event]),
    {reply, ok, StateName, State}.


handle_event(stop, _StateName, State) ->
    {stop, normal, State};
handle_event({send_invite, Request}, start, State) ->
    [Contact] = keylist:fetch('contact', Request#request.header),
    State0 = State#state{invite=Request, contact=Contact},
    case do_send_invite(Request, State0) of
	{ok, State1} ->
	    {next_state, outgoing, State1};
	{siperror, _Status1, _Reason1} ->
	    {stop, normal, State};
	R ->
	    logger:log(normal, "send_request failed: ~p", [R]),
	    {stop, normal, State}
    end;

handle_event(Request, StateName, State) ->
    error_logger:error_msg("Unhandled cast in ~p: ~p~n", [?MODULE, Request]),
    {next_state, StateName, State}.




handle_info({servertransaction_cancelled, Pid, _ExtraHeaders}, incoming=_StateName, #state{invite_pid=Pid}=State) ->
    %% TODO Send hangup
    logger:log(normal, "servertransaction_cancelled ~n", []),
    ok = siphelper:send_response(State#state.invite, 487, "Request Terminated"),
    {stop, normal, State};
handle_info({servertransaction_terminating, Pid}, incoming=StateName, #state{invite_pid=Pid}=State) ->
    %% Ignore
    logger:log(normal, "servertransaction_terminating ~n", []),
    {next_state, StateName, State};
handle_info(timeout, incoming=StateName, State) ->
    %% TODO Handle INVITE timeout
    ok = siphelper:send_response(State#state.invite, 408, "Request Timeout"),
    {next_state, StateName, State};
handle_info({branch_result, Pid, Branch, BranchState, #response{status=Status}=Response}, bye_sent=StateName, #state{bye_pid = Pid, bye_branch = Branch} = State) ->
    logger:log(normal, "branch_result: ~p ~p~n", [BranchState, Status]),
    if
        BranchState == completed, Status >= 200, Status =< 299 ->
	    logger:log(normal, "Terminate dialog: ~p ~p", [BranchState, Status]),
	    {stop, normal, State};
	BranchState == completed, Status >= 300, Status =< 699 ->
	    logger:log(normal, "Terminate dialog: ~p ~p", [BranchState, Status]),
            {stop, normal, State};
        true ->
            logger:log(normal, "IGNORING response '~p ~p ~s' to my BYE",
		       [BranchState, Status, Response#response.reason]),
            {next_state, StateName, State}
    end;
handle_info({branch_result, Pid, Branch, BranchState, #response{status=Status}=Response}, outgoing=StateName, #state{invite_pid = Pid, invite_branch = Branch} = State) ->
    {Skip, State1} = pred_skip_resp(BranchState, Status, Response, State),
    case Skip of
	false ->
	    handle_invite_result(Pid, Branch, BranchState, Response, StateName, State1);
	true ->
	    {next_state, StateName, State1}
    end;

handle_info({new_response, #response{status=Status}=Response, Origin, _LogStr}, StateName, State) when is_record(Origin, siporigin) ->
    %% FIXME retransmit ACK
    logger:log(normal, "200 OK retransmit received '~p ~s' to my invite",
	       [Status, Response#response.reason]),
    {next_state, StateName, State};

handle_info({new_request, FromPid, Ref, #request{method="ACK"} = _NewRequest, _Origin, _LogStrInfo}, StateName, State) ->
    %% Don't answer ACK
    %% TODO update dialog timeout?
    %% FIXME stop retransmission of 200 Ok
    FromPid ! {ok, self(), Ref},
%%     logger:log(normal, "Dialog received ACK"),
    {next_state, StateName, State};

handle_info({new_request, FromPid, Ref, NewRequest, _Origin, _LogStrInfo}, StateName, State) ->
    THandler = transactionlayer:get_handler_for_request(NewRequest),
    FromPid ! {ok, self(), Ref},
    {Action, NewDialog} = 
	case sipdialog:update_dialog_recv_request(NewRequest, State#state.dialog) of
	    {error, old_cseq} ->
		transactionlayer:send_response_handler(THandler, 500, "CSeq not higher than last requests");
	    {ok, NewDialog1} ->
		case NewRequest#request.method of
		    "BYE" ->                        
			%% answer BYE with 200 Ok
			transactionlayer:send_response_handler(THandler, 200, "Ok"),
			logger:log(normal, "Dialog ended by remote end (using BYE)"),
			%% TODO send hangup
			{stop, NewDialog1};
		    _ ->
			%% answer all unknown requests with 501 Not Implemented
			transactionlayer:send_response_handler(THandler, 501, "Not Implemented"),
			{next_state, NewDialog1}
		end
	end,

    case Action of
        next_state ->
            {next_state, StateName, State#state{dialog = NewDialog}};
        stop ->
            {stop, normal, State#state{dialog = NewDialog}}
    end;

handle_info({retry_invite, Request}, StateName, State) ->
    case do_send_invite(Request, State) of
	{ok, State1} ->
	    {next_state, StateName, State1};
	{siperror, _Status1, _Reason1} ->
	    {stop, normal, State};
	R ->
	    logger:log(normal, "send_request failed: ~p", [R]),
	    {stop, normal, State}
    end;

handle_info(Info, StateName, State) ->
    error_logger:error_msg("~p: Unhandled info in ~p ~p~n",
			   [?MODULE, Info, StateName]),
    {next_state, StateName, State}.


terminate(Reason, _StateName, _State) ->
    error_logger:error_msg("~p: Terminated ~p~n", [?MODULE, Reason]),
    terminated.


pred_skip_resp(BranchState, Status, Response, State) when BranchState == proceeding, Status >= 101, Status =< 199 ->
%	    Request = Status#state.invite,
    case sipheader:is_required("100rel", Response#response.header) of
	true ->
	    [Rseq_str] = keylist:fetch("rseq", Response#response.header),
	    Rseq = list_to_integer(Rseq_str),
	    Inv_rseq = State#state.invite_rseqno,
	    Dialog = State#state.dialog,
	    
	    logger:log(normal, "Rseq ~p ~p ~p~n", [Rseq_str, Rseq, Dialog]),
	    if
		Inv_rseq == undefined ->
		    logger:log(normal, "undefined rseq~n", []),
		    {ok, State1} = send_prack(Response, Rseq, State),
		    {false, State1};
		Inv_rseq + 1 == Rseq ->
		    logger:log(normal, "Next rseq~n", []),
		    {ok, State1} = send_prack(Response, Rseq, State),
		    {false, State1};
		true ->
		    logger:log(normal, "Not next rseq~n", []),
		    {true, State}
	    end;
	false ->
	    logger:log(normal, "no 100rel~n", []),
	    {false, State}
    end;
pred_skip_resp(_BranchState, _Status, _Response, State) ->
    {false, State}.


send_prack(Response, Rseq, State) ->
    Dialog = set_dialog_state_uac(Response, State),
    {ok, Request, NewDialog} = generate_new_request("PRACK", Dialog, State#state.contact),

%%     throw({send_prack, Dialog, Request}),

    Rack = lists:concat([Rseq, " ", State#state.invite_cseqno, " INVITE"]),
    Header1 = keylist:set("RAck", [Rack], Request#request.header),
    
    {ok, _Pid, _Branch} = siphelper:send_request(Request#request{header=Header1}),
    State1 = State#state{dialog=NewDialog, invite_rseqno=Rseq},

    % TODO add Pid to outgoing pid list
    {ok, State1}.


set_dialog_state_uac(Response, State) when is_record(Response, response),
					   is_record(State, state) ->
    case State#state.dialog of
	undefined ->
	    {ok, Dialog1} =
		sipdialog:create_dialog_state_uac(State#state.invite_pending, Response),
	    ok = sipdialog:register_dialog_controller(Dialog1, self()),
	    Dialog1;
	
	Dialog1 ->
 	    update_dialog_state_uac(Response, Dialog1)
    end.


update_dialog_state_uac(Response, Dialog) when is_record(Response, response),
						is_record(Dialog, dialog) ->
    %% Update route set of existing dialog
    RHeader = Response#response.header,
    Route = lists:reverse(keylist:fetch('record-route', RHeader)),
    Dialog1 = Dialog#dialog{route_set=Route},
    refresh_dialog_target_uac(Response, Dialog1).


refresh_dialog_target_uac(Response, Dialog) when is_record(Response, response),
						is_record(Dialog, dialog) ->
    %% Update route set of existing dialog
    RHeader = Response#response.header,
    RemoteTarget =
        case {Dialog#dialog.remote_target, keylist:fetch('contact', RHeader)} of
	    {undefined, []} -> undefined;
            {Contact1, []} -> Contact1;
            {_, [Contact1]} -> Contact1
        end,

    Dialog#dialog{remote_target=RemoteTarget}.


handle_invite_result(_Pid, _Branch, BranchState, #response{status=Status}=Response, StateName, State) ->
    logger:log(normal, "branch_result: ~p ~p~n", [BranchState, Status]),
    if
	%% TODO Handle all 101 <= Status <= 199
	BranchState == proceeding, Status == 180 ->
	    Dialog = set_dialog_state_uac(Response, State),
	    %% TODO Send to
	    State1 = State#state{dialog=Dialog},
            {next_state, StateName, State1};
        BranchState == terminated, Status >= 200, Status =< 299 ->
	    logger:log(normal, "Answered dialog: ~p ~p", [BranchState, Status]),
	    Dialog = set_dialog_state_uac(Response, State),

	    %% TODO send answered
	    {ok, Ack, Dialog1} =
		generate_new_request("ACK", Dialog, State#state.contact,
				     State#state.invite_cseqno),
	    {ok, _SendingSocket, _Dst, _TLBranch} = send_ack(Ack),
	    State1 = State#state{dialog=Dialog1},
	    {next_state, up, State1};

%%  	    {next_state, StateName, State};
	BranchState == completed, Status >= 300, Status =< 399 ->
            {stop, {siperror, Status, Response#response.reason}, State};

	BranchState == completed, State#state.invite_cseqno == 1, Status == 401 orelse Status == 407 ->
	    %% TODO add reason to drop
	    %% TODO fix looping

	    Lookup = fun(Realm, From, To) ->
			     error_logger:info_msg("~p: fun ~p ~p ~p~n", [?MODULE, Realm, From, To]),
 			     {ok, "2001", "test"}
		     end,
	    {ok, Auths, Changed} = siphelper:update_authentications(Response, Lookup, State#state.auths),

	    Retry_after = siphelper:get_retry_after(Response) * 1000,

	    case Changed of
		false ->
		    {stop, {siperror, Status, Response#response.reason}, State};
		true ->
		    Request = State#state.invite_pending,
		    {ok, Retry_timer} = timer:send_after(Retry_after, {retry_invite, Request}),
		    State1 = State#state{retry_timer=Retry_timer, auths=Auths},
		    {next_state, StateName, State1}
	    end;
%% 	    {ok, Request} = siphelper:add_authorization(State#state.invite, Response),

	BranchState == completed, Status >= 400, Status =< 699 ->
            {stop, {siperror, Status, Response#response.reason}, State};
        true ->
            logger:log(normal, "IGNORING response '~p ~p ~s' to my invite",
		       [BranchState, Status, Response#response.reason]),
            {next_state, StateName, State}
    end.



send_ack(Request) ->
    Branch = siprequest:generate_branch(),
    send_ack(Request, Branch).

send_ack(Request, Branch) ->
    Route = keylist:fetch('route', Request#request.header),
    TargetURI = Request#request.uri,
    Dst = case Route of
	      [] ->
		  [Dst1 | _] = sipdst:url_to_dstlist(TargetURI, 500, TargetURI),
		  Dst1;
	      [FirstRoute | _] ->
		  [FRC] = contact:parse([FirstRoute]),
		  FRURL = sipurl:parse(FRC#contact.urlstr),
		  [Dst1 | _] = sipdst:url_to_dstlist(FRURL, 500, TargetURI),
		  Dst1
	  end,

    case transportlayer:send_proxy_request(none, Request, Dst,
					   ["branch=" ++ Branch]) of
	{ok, SendingSocket, TLBranch} ->
	    {ok, SendingSocket, Dst, TLBranch};
	_ ->
	    error
    end.

do_send_invite(Request, State) ->
    [Contact] = keylist:fetch('contact', Request#request.header),

    %% TODO set cseqno

    Header = Request#request.header,
    CSeq = State#state.invite_cseqno + 1,
    Header1 = keylist:set("CSeq", [lists:concat([CSeq, " ", Request#request.method])], Header),
    Request1 = Request#request{header=Header1},
    {ok, Request2} = siphelper:add_authorization(Request1, State#state.auths),

    case siphelper:send_request(Request2) of
	{ok, Pid, Branch} ->
	    
	    State1 = State#state{invite_branch=Branch,
				 invite_pid=Pid,
				 invite_pending=Request1,
				 invite_cseqno=CSeq,
				 contact=Contact
				},
	    {ok, State1};
	R ->
	    R
    end.
