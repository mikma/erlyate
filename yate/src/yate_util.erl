%%%
%%% @doc       Yate utilities
%%% @author    Mikael Magnusson <mikma@users.sourceforge.net>
%%% @copyright 2006-2008 Mikael Magnusson
%%%

-module(yate_util).

-export([seconds/0, dict_store_list/2]).

-include("yate.hrl").

%%--------------------------------------------------------------------
%% @spec seconds() -> integer()
%% @doc Return number of seconds from epoch
%% @end
%%--------------------------------------------------------------------
seconds() ->
    {MegaSecs, Secs, _MicroSecs} = now(),
    MegaSecs * 1000000 + Secs.


%%--------------------------------------------------------------------
%% @spec dict_store_list(List, Dict) -> Dict1
%%         List = [{Key, Value}]
%%         Key = term()
%%         Value = term()
%%         Dict = term()
%%         Dict1 = term()
%% @doc Store a list of key/value pairs in a dictionary.
%% @end
%%--------------------------------------------------------------------
dict_store_list([], Dict) ->
    Dict;
dict_store_list([{Key, Value}|R], Dict) ->
    dict_store_list(R, dict:store(Key, Value, Dict)).
