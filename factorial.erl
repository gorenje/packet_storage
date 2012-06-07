%%%-------------------------------------------------------------------
%%% File    : factorial.erl
%%% Author  : Gerrit Riessen <gerrit.riessen@open-source-consultants.de>
%%% Description : 
%%%
%%% Created : 23 Jul 2008 by Gerrit Riessen <gerrit.riessen@open-source-consultants.de>
%%%-------------------------------------------------------------------
-module(factorial).

-export([start/0,init/1,fact/1]).

fact(0) ->
    1;
fact(N) ->
    fact(N-1) * N.

start() ->
    spawn(factorial, init, [self()]).

init(From) ->
    loop(From).

loop(From) ->
    receive
        _ ->
            loop(From)
    end.
