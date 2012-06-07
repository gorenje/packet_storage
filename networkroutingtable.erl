%%%-------------------------------------------------------------------
%%% File    : networkroutingtable.erl
%%% Author  : Gerrit Riessen <gerrit.riessen@open-source-consultants.de>
%%% Description : implementation of a routing table algorithm
%%%
%%% Created : 22 Aug 2008 by Gerrit Riessen <gerrit.riessen@open-source-consultants.de>
%%%-------------------------------------------------------------------
-module(networkroutingtable).
-export([test/0,addr_to_node/2,add_route/2,del_route/2,all_routes/1,domain_match/2]).
-import(lists,[merge/3,flatten/1,filter/2]).

%% assume that a routing table is a list of network addresses, node tuples
%% these are matched in turn against the address. ordering is that the 
%% first entry (i.e. should be most specific) of the list to the last 
%% (i.e. least specific)
%% E.g. 
%% [ { [127,0,0,1], Pid },
%%   { [127,1,1],   Pid },
%%   { [127,1,2],   Pid },
%%   { [],  DefaultGateway } ]
%%

addr_to_node(_,[])                          ->
    addr_to_node(bal,[],bad);
addr_to_node(To,[{Addr,Node}|RoutingTable]) ->
    addr_to_node(To,RoutingTable,nm(To,Addr,Node)).

addr_to_node(To,[{Addr,Node}|RoutingTable],{nomatch,_}) ->
    addr_to_node(To,RoutingTable,nm(To,Addr,Node));
addr_to_node(_,_,{match,Node})                          ->
    Node;
addr_to_node(_,[],_)                                    ->
    nomatchingnode.

add_route(NewRoute,RoutingTable) ->
    merge(fun({Addr,_},{Addr2,_}) -> length(Addr) > length(Addr2) end, RoutingTable, [NewRoute]).

del_route({Addr,_},RoutingTable) ->
    del_route(Addr,RoutingTable);
del_route(Addr,RoutingTable) ->
    filter( fun({Addr2,_}) -> Addr =/= Addr2 end, RoutingTable ).

%% all routes minus the default gateways
all_routes(RoutingTable) ->                     
    del_route([],RoutingTable).
    
%% nm == network match
nm([Ta,  Tb, Tc, Td], [Ta,Tb,Tc,Td],Node) -> {match,Node}; %% complete ip match
nm([Ta,  Tb, Tc,  _], [Ta,Tb,Tc],Node)    -> {match,Node}; %% level c match
nm([Ta,  Tb,  _,  _], [Ta,Tb],Node)       -> {match,Node}; %% level b match
nm([Ta,   _,  _,  _], [Ta],Node)          -> {match,Node}; %% level a match
nm([ _,   _,  _,  _], [],Node)            -> {match,Node}; %% default gateway
nm(_,_,Node)                              -> {nomatch,Node}.

%% this returns true if the domain matches a specific ip or
%% a domain matches another domain
%% Arguments are interpreted as (it's only used twice):
%%    1. PacketTo   RoutingTableRoute
%%    2. PacketTo   AddressOfNode
%% 
domain_match([],            [ _, _, _, _]) -> true;
domain_match([],            [ _, _, _])    -> true;
domain_match([],            [ _, _])       -> true;
domain_match([],            [ _])          -> true;

domain_match([Da],          [Da, _, _, _]) -> true;
domain_match([Da],          [Da,_,_])      -> true;
domain_match([Da],          [Da,_])        -> true;
domain_match([Da],          [Da])          -> true;

domain_match([Da,Db],       [Da,Db, _, _]) -> true;
domain_match([Da,Db],       [Da,Db,_])     -> true;
domain_match([Da,Db],       [Da,Db])       -> true;
domain_match([Da, _],       [Da])          -> true;

domain_match([Da,Db,Dc],    [Da,Db,Dc, _]) -> true;
domain_match([Da,Db,Dc],    [Da,Db,Dc])    -> true;
domain_match([Da,Db, _],    [Da,Db])       -> true;
domain_match([Da, _, _],    [Da])          -> true;

domain_match([Da,Db,Dc,Dd], [Da,Db,Dc,Dd]) -> true;
domain_match([Da,Db,Dc, _], [Da,Db,Dc])    -> true;
domain_match([Da,Db, _, _], [Da,Db])       -> true;
domain_match([Da, _, _, _], [Da])          -> true;

domain_match(_,_)                          -> false.
     
%%
%% do some tests to check things are working
%%
test() ->
    RoutingTable = [ {[127,0,0,2], node1},
                     {[127,0,1,2], node2},
                     {[127,0,1],   node3},
                     {[127],       node4},
                     {[],          defgw} ],
    NodesWithOutDefGw = [ {[127,0,0,2], node1},
                          {[127,0,1,2], node2},
                          {[127,0,1],   node3},
                          {[127],       node4} ],
    NodesWithOutDefGw = all_routes(RoutingTable),
    node1 = addr_to_node([127,0,0,2], RoutingTable),
    node2 = addr_to_node([127,0,1,2], RoutingTable),
    node3 = addr_to_node([127,0,1,3], RoutingTable),
    node3 = addr_to_node([127,0,1,23], RoutingTable),
    node3 = addr_to_node([127,0,1,13], RoutingTable),
    node3 = addr_to_node([127,0,1,6], RoutingTable),
    node3 = addr_to_node([127,0,1,7], RoutingTable),
    node4 = addr_to_node([127,1,11,2], RoutingTable),
    node4 = addr_to_node([127,13,1,2], RoutingTable),
    node4 = addr_to_node([127,11,21,2], RoutingTable),
    node4 = addr_to_node([127,1,21,2], RoutingTable),
    node4 = addr_to_node([127,11,1,2], RoutingTable),
    defgw = addr_to_node([198,0,0,2], RoutingTable),
    defgw = addr_to_node([112,123,0,2], RoutingTable),
    RoutingTableNoDefault =  [ {[127,0,0,2], node1},
                               {[127,0,1,2], node2},
                               {[127,0,1],   node3},
                               {[127],       node4}],
    node1 = addr_to_node([127,0,0,2], RoutingTableNoDefault),
    node2 = addr_to_node([127,0,1,2], RoutingTableNoDefault),
    node3 = addr_to_node([127,0,1,3], RoutingTableNoDefault),
    node3 = addr_to_node([127,0,1,23], RoutingTableNoDefault),
    node3 = addr_to_node([127,0,1,13], RoutingTableNoDefault),
    node3 = addr_to_node([127,0,1,6], RoutingTableNoDefault),
    node3 = addr_to_node([127,0,1,7], RoutingTableNoDefault),
    node4 = addr_to_node([127,1,11,2], RoutingTableNoDefault),
    node4 = addr_to_node([127,13,1,2], RoutingTableNoDefault),
    node4 = addr_to_node([127,11,21,2], RoutingTableNoDefault),
    node4 = addr_to_node([127,1,21,2], RoutingTableNoDefault),
    node4 = addr_to_node([127,11,1,2], RoutingTableNoDefault),
    nomatchingnode = addr_to_node([198,0,0,2], RoutingTableNoDefault),
    nomatchingnode = addr_to_node([112,123,0,2], RoutingTableNoDefault),
    true  = domain_match( [127,1,1,2], [127,1,1,2] ),
    false = domain_match( [127,1,1,2], [127,1,1] ),
    false = domain_match( [127,1,1,2], [127,1] ),
    false = domain_match( [127,1,1,2], [127] ),
    false = domain_match( [127,1,1,2], [] ),
    true  = domain_match( [127,1,1], [127,1,1,2] ),
    true  = domain_match( [127,1,1], [127,1,1] ),
    false = domain_match( [127,1,1], [127,1] ),
    false = domain_match( [127,1,1], [127] ),
    false = domain_match( [127,1,1], [] ),
    true  = domain_match( [127,1], [127,1,1,2] ),
    true  = domain_match( [127,1], [127,1,1] ),
    true  = domain_match( [127,1], [127,1] ),
    false = domain_match( [127,1], [127] ),
    false = domain_match( [127,1], [] ),
    true  = domain_match( [127], [127,1,1,2] ),
    true  = domain_match( [127], [127,1,1] ),
    true  = domain_match( [127], [127,1] ),
    true  = domain_match( [127], [127] ),
    false = domain_match( [127], [] ),
    test_successful.
