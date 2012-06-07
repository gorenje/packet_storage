%%%-------------------------------------------------------------------
%%% File    : png-scanner.erl
%%% Author  : Gerrit Riessen <gerrit.riessen@open-source-consultants.de>
%%% Description : 
%%%
%%% Created : 29 Jul 2008 by Gerrit Riessen <gerrit.riessen@open-source-consultants.de>
%%%-------------------------------------------------------------------
-module('pngscanner').
-compile(export_all).
%%-export([is_valid/1]).
-define(UINT, unsigned-integer).
-define(DWORD, 4/binary).
-define(LONG, 32/?UINT).
-define(WORD, 16).
-define(BYTE, 8/?UINT).
-define(DONTCARE, _/binary).


%% PNG Header
%% <<137, 80, 78, 71, 13, 10, 26, 10>>

strip_off_header(<<137, 80, 78, 71, 13, 10, 26, 10, Rest/binary>>) ->
    {ok, Rest}.

handle_type(<<73,72,68,82>>,Data) ->
    io:format("Handling IHDR~n"),
    <<Width:?LONG, Height:?LONG, BitDepth:?BYTE, ColorType:?BYTE, CompMeth:?BYTE, FilterMeth:?BYTE, InterlaceMeth:?BYTE>> = Data,
    io:format("Width ~w, Height ~w, Depth: ~w, Color: ~w, Compression: ~w, Filter: ~w, Inter: ~w~n",[Width,Height,BitDepth,ColorType,CompMeth,FilterMeth,InterlaceMeth]);

handle_type(<<73,68,65,84>>,Data) ->
    io:format("IDAT~n"),
    <<CINFO:4/?UINT,CM:4/?UINT, FLEVEL:2/?UINT, FDICT:1/?UINT, FCHECK:5/?UINT, ZlibBlock/binary>> = Data,
    io:format( "Meth: ~p,~p~n", [CM,CINFO]),
    io:format( "FDICT: ~p, FLEVEL: ~p, FCHECK: ~p~n", [FDICT,FLEVEL,FCHECK]),
    <<SOMEMORE:5/?UINT, BTYPE:2/?UINT, BFINAL:1/?UINT, ?DONTCARE>> = ZlibBlock,
    io:format( "ZHdr: ~p, ~p, ~p~n", [BFINAL, BTYPE, SOMEMORE]),
    [];

handle_type(_,_) ->
    [].

obtain_next_chunk(<<73,69,78,68>>,_) -> [];
obtain_next_chunk(LastType, <<Length:32/big-unsigned-integer, Type:4/binary, Rest/binary>>) ->
    io:format("Length: ~w, Type: ~w ~n", [Length, Type]),
    <<Tc1:?BYTE, Tc2:?BYTE, Tc3:?BYTE, Tc4:?BYTE>> = Type,
    io:format("Type: ~p~n", [[Tc1,Tc2,Tc3,Tc4]]),
    <<Data:Length/binary, Crc:32/integer, MoreData/binary>> = Rest,
    handle_type(Type,Data),
    obtain_next_chunk(Type, MoreData).
  
scan_whole_png(Bin) ->
    {ok,Data} = strip_off_header(Bin),
    obtain_next_chunk(<<0,0,0,0>>,Data).
