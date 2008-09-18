%%%
%%% @doc       Yate application module
%%% @author    Mikael Magnusson <mikma@users.sourceforge.net>
%%% @copyright 2006-2008 Mikael Magnusson
%%%
-module(yate_app).

-behaviour(application).

%% application callbacks
-export([start/2, stop/1]).

%% api:s
-export([start/0]).

%% application callbacks

%% @private
start(normal, []) ->
    start().

%% @private
stop(_State) ->
    ok.


%%--------------------------------------------------------------------
%% @spec start() -> Result
%%           Result = {ok, Pid} | {error, Reason}
%% @doc Start top supervisor of the Yate application
%% @end
%%--------------------------------------------------------------------
start() ->
    yate_sup:start_link().
