%%%-------------------------------------------------------------------
%%% File    : benchmark.erl
%%% Author  : Gerrit Riessen <gerrit.riessen@open-source-consultants.de>
%%% Description : ring benchmark program as suggested in programming erlang
%%%
%%% Created : 15 Aug 2008 by Gerrit Riessen <gerrit.riessen@open-source-consultants.de>
%%%-------------------------------------------------------------------
-module(benchmark).
-compile(export_all).
%% -export([start/0,init/1]).

start(ProcessCnt) ->
    Master = spawn(benchmark, master, [self()]),
    Master ! {neighbour, spawn(benchmark, init, [Master, ProcessCnt])},
    Master.

init(Master, 0, Neighbour) ->
    spawn(benchmark, slave, [Master,Neighbour]);

init(Master, ProcessCnt, Neighbour) ->
    Neighbour2 = spawn(benchmark, slave, [Master,Neighbour]),
    init(Master, ProcessCnt-1, Neighbour2).

init(Master,ProcessCnt) ->
    Pid = init(Master,ProcessCnt-1,self()),
    slave(Master, Pid).

master(From) ->
    receive
        {neighbour, Neighbour} ->
            master(Neighbour);
        {msg,Cnt,Token} ->
            io:format( "Start with: ~w~n", [Token]),
            statistics(wall_clock),
            statistics(runtime),
            From ! {msg,Cnt,Token},
            master(From);
        {done,Token,Sender} ->
            {_,WallTime} = statistics(wall_clock),
            {_,RunTime} = statistics(runtime),
            io:format( "Done with: ~w from ~p took W: ~w R: ~w~n", [Token,Sender,WallTime,RunTime]),
            master(From);
        Any ->
            io:format( "Received: ~p~n",[Any]),
            master(From)
    end.
    
slave(Master,Neighbour) ->
    receive
        {msg, 0, Token} ->
            Master ! {done,Token,self()},
            slave(Master,Neighbour);
        {msg, Cnt, Token} ->
            Neighbour ! {msg, Cnt-1, Token},
            slave(Master,Neighbour);
        Any ->
            io:format( "Received: ~p~n",[Any]),
            slave(Master,Neighbour)
    end.
