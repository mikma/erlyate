%%%
%%% @doc       Yate demo application module
%%% @author    Mikael Magnusson <mikma@users.sourceforge.net>
%%% @copyright 2006 Mikael Magnusson
%%%
-module(yate_demo_app).

-behaviour(application).

%% application callbacks
-export([start/2, stop/1]).

%% api:s
-export([start/0]).

%% application callbacks
start(normal, []) ->
    start().

stop(_State) ->
    ok.

%%--------------------------------------------------------------------
%% @spec start() -> Result
%%           Result = {ok, Pid} | {error, Reason}
%% @doc Start demo application
%% @end
%%--------------------------------------------------------------------
start() ->
    yate_demo_sup:start_link().
