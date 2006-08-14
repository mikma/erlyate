%%%
%%% @doc       Demo
%%% @author    Mikael Magnusson <mikma@users.sourceforge.net>
%%% @copyright 2006 Mikael Magnusson
%%%
-module(yate_demo).

-behaviour(gen_server).

-include("yate.hrl").

%% api
-export([start_link/1, stop/0, run/0]).

%% gen_server callbacks
-export([init/1,
	 code_change/3,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2]).

-record(sstate, {handle, client}).

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
    {ok, Client} = yate:connect(?HOST, ?PORT),
    {ok, _Pid} = start_link(Client),
    ok.

    
%%--------------------------------------------------------------------
%% @spec start_link() -> Result
%%           Result = {ok, Pid} | ignore | {error, Error}
%%           Pid = pid()
%%           Error = {already_started, Pid} | term()
%% @doc create demo process
%% @end
%%--------------------------------------------------------------------
start_link(Client) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Client], []).


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
init([Client]) ->
    {ok, Handle} = yate:open(Client),
    error_logger:info_msg("install call.route 99991009~n"),
    ok = yate:install(Handle, call.route, 
		      fun(Cmd) ->
			      dict:fetch(called, Cmd#command.keys) == "99991009"
		      end),
    ok = yate:install(Handle, call.execute,
		      fun(Cmd) ->
			      Callto = dict:fetch(callto, Cmd#command.keys),
			      Callto == "erl/test"
		      end),
    {ok, #sstate{handle=Handle, client=Client}}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_call(Request, _From, State) ->
    error_logger:error_msg("Unhandled call: ~p~n", [Request]),
    {reply, ok, State}.


handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(Request, State) ->
    error_logger:error_msg("Unhandled cast: ~p~n", [Request]),
    {noreply, State}.


handle_info({yate, Dir, Cmd, From}, State) ->
    handle_command(Cmd#command.type, Dir, Cmd, From, State);
handle_info(Info, State) ->
    error_logger:error_msg("Unhandled info: ~p~n", [Info]),
    {noreply, State}.


terminate(_Reason, State) ->
    Handle = State#sstate.handle,
    yate:uninstall(Handle, call.route),
    terminated.


handle_command(message, req, Cmd, From, State) ->
    handle_message((Cmd#command.header)#message.name, Cmd, From, State);
handle_command(message, ans, Cmd, _From, State) ->
    Header = Cmd#command.header,
    error_logger:error_msg("Unhandled answer message in ~p: ~p~n", [?MODULE, Header#message.name]),
    {noreply, State};
handle_command(Type, req, Cmd, _From, State) ->
    Handle = State#sstate.handle,
    yate:ret(Handle, Cmd, false),
    error_logger:error_msg("Unhandled request in ~p: ~p~n", [?MODULE, Type]),
    {noreply, State};
handle_command(Type, ans, _Cmd, _From, State) ->
    error_logger:error_msg("Unhandled answer in ~p: ~p~n", [?MODULE, Type]),
    {noreply, State}.


handle_message(call.execute, Cmd, _From, State) ->
    Callto = dict:fetch(callto, Cmd#command.keys),
    Id = dict:fetch(id, Cmd#command.keys),
    Handle = State#sstate.handle,
    NewKeys = dict:store(callto, "dumb/", Cmd#command.keys),
    error_logger:info_msg("Call execute to ~p.~n", [Callto]),

    {ok, _Pid} = yate_demo_call:start_link(State#sstate.client, Id),
    yate:ret(Handle, Cmd#command{keys=NewKeys}, false),
    {noreply, State};
handle_message(call.route, Cmd, From, State) ->
    handle_call_route(dict:fetch(called, Cmd#command.keys), Cmd, From, State);
handle_message(Type, Cmd, _From, State) ->
    Handle = State#sstate.handle,
    yate:ret(Handle, Cmd, false),
    error_logger:error_msg("Unhandled message request: ~p~n", [Type]),
    {noreply, State}.

handle_call_route("99991009", Cmd, _From, State) ->
    Id = dict:fetch(id, Cmd#command.keys),
    {ok, _Pid} = yate_demo_call:start_link(State#sstate.client, Id),
    Handle = State#sstate.handle,
    yate:ret(Handle, Cmd, true, "dumb/"),
%%    yate:ret(From, Cmd, true, "tone/dial"),
    {noreply, State};
handle_call_route(Called, Cmd, _From, State) ->
    Handle = State#sstate.handle,
    yate:ret(Handle, Cmd, false),
    error_logger:error_msg("Unhandled call.route to: ~p~n", [Called]),
    {noreply, State}.


%% sendroute() ->
%%     {ok, RetValue, _RetCmd} =
%% 	yate:send_msg(Pid, call.route,
%% 		      [{id, "erlang/bogus"},
%% 		       {module, erlang},
%% %%		       {status, incoming},
%% %%		       {address, "0.0.0.0:0"},
%% %%		       {answered, false},
%% 		       {called, "99991001"},
%% 		       {caller, anonymous},
%% 		       {callername, "Anonymous"}]),
%%     error_logger:info_msg("call.route ~p~n", [RetValue]),
%%     ok.

%% Example: Simple IVR

%% Calls to "ivr" will get connected to keyecho application that echos pressed keys. Replace "%s.gsm" with your path to digits recordings.

%% from twisted.internet import reactor,
%%  defer
%% from yaypm import TCPDispatcherFactory

%% def route(yate):
%%     def on_route(route):
%%         callid = route["id"]
%%         route.ret(True, "dumb/")

%%         def on_execute(execute):
%%             yate.msg("call.answered",
%%                      {"id": execute["targetid"],
%%                       "targetid": execute["id"]}).enqueue()
%%             print "Call %s answered." % callid
%%             def on_dtmf(dtmf):
%%                 print "Dtmf %s received." % dtmf["text"]
%%                 yate.msg("chan.masquerade",
%%                     {"message" : "chan.attach",                    
%%                      "id": dtmf["targetid"],
%%                      "source": "wave/play/./sounds/digits/pl/%s.gsm" % \
%%                      dtmf["text"]}).enqueue()
%%                 yate.onmsg("chan.dtmf",
%%                     lambda m : m["id"] == dtmf["id"]).addCallback(on_dtmf)
%%                 dtmf.ret(True)
%%             dtmf = yate.onmsg("chan.dtmf",
%%                 lambda m : m["id"] == execute["id"])
%%             dtmf.addCallback(on_dtmf)            

%%         execute = yate.onwatch("call.execute",
%%             lambda m : m["id"] == callid)
%%         execute.addCallback(on_execute)
%%         yate.onmsg("call.route").addCallback(on_route)

%%     yate.onmsg("call.route", 
%%         lambda m : m["called"] == "ivr").addCallback(on_route)

%% if __name__ == '__main__':

%%     f = TCPDispatcherFactory(route)
%%     reactor.connectTCP("localhost", 5039, f)

%%     reactor.run()    

