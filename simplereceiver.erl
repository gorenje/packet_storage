%%%-------------------------------------------------------------------
%%% File    : simplereceiver.erl
%%% Author  : Gerrit Riessen <gerrit.riessen@open-source-consultants.de>
%%% Description : 
%%%
%%% Created : 10 Aug 2008 by Gerrit Riessen <gerrit.riessen@open-source-consultants.de>
%%%-------------------------------------------------------------------
-module(simplereceiver).
-compile(export_all).

%% to use this bridge:
%%   simplereceiver:start().
%%   simplereceiver:send_msg(three, <<"this is a stirng">>).
%% watch out for hte strange string encoding... unfortunately required.
%% Update: changed the ruby code to always accept an Any value, so that
%% anything can be sent over the wire. Although the type specific rules (just
%% like in erlang) will match before any Any-rule.
start() ->
    spawn(fun()->
       register(simpletest,self()),
       Cmd = "ruby ./simplereceiver.rb",
       Port = open_port({spawn, Cmd}, [{packet, 4}, binary, use_stdio,exit_status]),
       port_loop(Port)
    end).
   
send_msg(Msg, Val) ->
    simpletest ! {command, self(), Msg, Val},
    io:format( "Sent command~n",[]),
    receive
        {ok, ReturnMsg} ->
            io:format( "Got rtesl~n"),
            ReturnMsg
    end.

port_loop(Port) ->
    receive 
        {command, Caller, Msg, Val} ->
            io:format("Got comamnd : ~w~n", [Msg]),
            CmdData = term_to_binary({Msg, Val}),
            Port ! {self(), {command, CmdData}},
            Rval = get_result(Port),
            Caller ! {ok,Rval},
            port_loop(Port)
    end.
    
get_result(Port) ->
    io:format("get result~n",[]),
    receive 
        {Port, {data, Data}} ->
            io:format("get result got something: ~w~n", [Data]),
            {ok, Bin} = binary_to_term(Data),
            Bin;
        {'EXIT',Port, Reason} ->
            io:format("Exit ~w~n",[Reason])
    end.

