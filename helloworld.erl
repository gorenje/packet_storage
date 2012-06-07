%%%-------------------------------------------------------------------
%%% File    : helloworld.erl
%%% Author  : Gerrit Riessen <gerrit.riessen@open-source-consultants.de>
%%% Description : 
%%%
%%% Created : 22 Aug 2008 by Gerrit Riessen <gerrit.riessen@open-source-consultants.de>
%%%-------------------------------------------------------------------
-module(helloworld).

-export([start/0,init/1]).


start() ->
    spawn(helloworld, init, [self()]).

init(From) ->
    loop(From).

loop(From) ->
    receive
        {returnmsg,From} ->
            From ! {gotmsg,hello},
            loop(From);
        {hello,value} ->
            io:format( "goodbye~n",[]),
            loop(From);
        {hello,Value} ->
            io:format("got value: ~w~n",[Value]),
            loop(From);
        Any ->
            io:format("hello world: ~w~n",[Any]),
            loop(From)
    end.
