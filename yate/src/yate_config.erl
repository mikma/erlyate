%%%-------------------------------------------------------------------
%%% File    : yate_config.erl
%%% Author  : Mikael Magnusson <mikael@fowley.hem.za.org>
%%% Description : 
%%%
%%% Created :  4 Jul 2010 by Mikael Magnusson <mikael@fowley.hem.za.org>
%%%-------------------------------------------------------------------
-module(yate_config).

-export([read/1, get/3, get_extmodule_listener/2]).

-record(config, {sections}).

read(Filename) ->
    {ok, Io} = file:open(Filename, [read, raw, {read_ahead, 1024}]),
    #config{sections=read_file(Io, none, [], [])}.

get(Config, Section, Key) ->
    Keylist = get_section(Config, Section),
    get_value(Keylist, Key).

get_extmodule_listener(Config, Listener) ->
    Keylist = get_section(Config, "listener " ++ Listener), 
    Type = list_to_existing_atom(get_value(Keylist, "type")),
    Addr = get_value(Keylist, "addr"),
    Port = list_to_integer(get_value(Keylist, "port")),
    {Type, Addr, Port}.

get_value(Keylist, Key) ->
    case lists:keysearch(Key, 1, Keylist) of
        {value, {_, Value}} ->
            Value;
        false ->
            throw(key_not_found)
    end.    

get_section(Config, Section) ->
    case lists:keysearch(Section, 1, Config#config.sections) of
        {value, {_, Keylist}} ->
            Keylist;
        false ->
            throw(section_not_found)
    end.

read_file(Io, SectionName, SectionAcc, Acc) ->
    case file:read_line(Io) of
        {ok, "[" ++ _ = Data} ->
            [SectionName1] = string:tokens(Data, "[]\n"),
            Acc1 =
                case SectionName of
                    none -> Acc;
                    _ -> [{SectionName, SectionAcc} | Acc]
                end,
            read_file(Io, SectionName1, [], Acc1);
        {ok, Line} ->
            SectionAcc1 = parse_line(Line, SectionAcc),
            read_file(Io, SectionName, SectionAcc1, Acc);
        eof ->
            Acc
    end.

parse_line(Line, SectionAcc) ->
    parse_line(Line, [], SectionAcc).

parse_line("\n", [], SectionAcc) ->
    SectionAcc;
parse_line("#" ++ _, [], SectionAcc) ->
    %% Comment only line
    SectionAcc;
parse_line(";" ++ _, [], SectionAcc) ->
    %% Comment only line
    SectionAcc;
parse_line("=" ++ Rest, KeyAcc, SectionAcc) when KeyAcc =/= "" ->
    parse_value(Rest, lists:reverse(KeyAcc), SectionAcc);
parse_line([C | Rest], KeyAcc, SectionAcc) ->
    parse_line(Rest, [C | KeyAcc], SectionAcc).

parse_value(Line, Key, SectionAcc) ->
    [{trim(Key), trim(Line)} | SectionAcc].

trim(Line) ->
    trim_right(trim_left(Line)).

trim_left(" " ++ Line) ->
    trim_left(Line);
trim_left("\n" ++ Line) ->
    trim_left(Line);
trim_left(Line) ->
    Line.

trim_right(Line) ->
    lists:reverse(trim_left(lists:reverse(Line))).
