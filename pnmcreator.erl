%%%-------------------------------------------------------------------
%%% File    : pnmcreator.erl
%%% Author  : Gerrit Riessen <gerrit.riessen@open-source-consultants.de>
%%% Description : 
%%%
%%% Created :  3 Aug 2008 by Gerrit Riessen <gerrit.riessen@open-source-consultants.de>
%%%-------------------------------------------------------------------
-module(pnmcreator).
-compile(export_all).
-import(math, [sqrt/1]).

%% use this in the following way:
%%   {ok,Bin1} = file:read_file('pi.png').
%%   pnmcreator:create(Bin1).
%% that's it.
-define(PNM_HEADER,["P3",10,"# Created by pnmcreated.erl",10]).

floor(Int,Float) when Int < Float ->
    floor(Int + 1, Float);
floor(Int,Float) when Int > Float ->
    Int - 1.

floor(Float) ->
    floor(1,Float).

dimension(ByteSize)->
    Sqrt = sqrt(ByteSize / 3),
    {ok, floor(Sqrt), floor(Sqrt)}.

create_data(Data, <<>>) ->    Data;
create_data(Data, <<_Red:8/unsigned-integer >>) ->    Data;
create_data(Data, <<_Red:8/unsigned-integer, _Green:8/unsigned-integer >>) ->    Data;
create_data(Data, <<Red:8/unsigned-integer, Green:8/unsigned-integer, Blue:8/unsigned-integer, Rest/binary>>) ->
    create_data([io_lib:format("~w~n~w~n~w~n",[Red,Green,Blue])|Data], Rest).

create(Bin) ->
    {ok,SizeX,SizeY} = dimension(size(Bin)),
    {ok,Output} = file:open("output.ppm", write),
    io:format(Output, ?PNM_HEADER, []),
    io:format(Output, "~w ~w~n", [SizeX,SizeY]),
    io:format(Output,lists:reverse(create_data([],Bin)),[]).
