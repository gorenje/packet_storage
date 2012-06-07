%%%-------------------------------------------------------------------
%%% File    : networkcreate.erl
%%% Author  : Gerrit Riessen <gerrit.riessen@open-source-consultants.de>
%%% Description : helpers for creating different networks
%%%
%%% Created : 23 Aug 2008 by Gerrit Riessen <gerrit.riessen@open-source-consultants.de>
%%%-------------------------------------------------------------------
-module(networkcreate).

-export([adomain/1,adomain/2,adomain/4,adomain/5]).

-import(environmenthelpers, [getenv/2,setenv/3,delenv/2]).
-import(lists,[flatten/1,nth/2]).
-import(io_lib,[format/2]).

%% Create a level A network and the corresponding routing tables
adomain([Ta])                           -> adomain([Ta],2,2,2).
adomain([Ta],BCnt,CCnt,DCnt)            -> adomain(node(),[Ta],BCnt,CCnt,DCnt).
adomain(RemoteNode,[Ta])                -> adomain(RemoteNode,[Ta],2,2,2).
adomain(RemoteNode,[Ta],BCnt,CCnt,DCnt) ->
    case whereis(nomatchingnode) of
        undefined -> 
            {_,Node} = new_node(RemoteNode,[0,0,0,0]),
            register(nomatchingnode, Node);
        _ -> []
    end,
    Nodes = flatten([ bdomain(RemoteNode,[Ta,X],CCnt,DCnt) || X <- lists:seq(1,BCnt)]),
    {[Ta],Gw} = basic_routing_table([Ta],BCnt,CCnt,DCnt,Nodes),
    {Gw, Nodes}.

%% wrapper for the network:new_node
new_node(RemoteNode,Ip) -> {Ip, network:new_node(RemoteNode,
                                                 flatten(format("Node-~w-~w-~w-~w",
                                                                [nth(1,Ip),nth(2,Ip),nth(3,Ip),nth(4,Ip)])),Ip)}.
cdomain(RemoteNode,[Ta,Tb,Tc],DCnt) ->
    [ new_node(RemoteNode,[Ta,Tb,Tc,X]) || X <- lists:seq(1,DCnt) ].

bdomain(RemoteNode,[Ta,Tb],CCnt,DCnt) ->
    flatten([ cdomain(RemoteNode,[Ta,Tb,X],DCnt) || X <- lists:seq(1,CCnt)]).

%% level A routing table
%% here [Ta,1] Gateway for the [Ta] network
basic_routing_table([Ta],BCnt,CCnt,DCnt,Nodes) ->
    Gateways = [ basic_routing_table([Ta,X],CCnt,DCnt,Nodes) || X <- lists:seq(1,BCnt) ],
    Gw = getenv([Ta,1],Gateways),
    Fun = fun(X) ->
                  Node = getenv([Ta,X],Gateways),
                  Node ! {add,{[],Gw}},
                  Gw ! {add,{[Ta,X],Node}}
          end,
    [ Fun(X) || X <- lists:seq(2,BCnt) ],
    { [Ta], Gw }.

%% level B routing table
%% Gateway for the [Ta,Tb] network is [Ta,Tb,1].
basic_routing_table([Ta,Tb],CCnt,DCnt,Nodes) ->
    Gateways = [ basic_routing_table([Ta,Tb,X],DCnt,Nodes) || X <- lists:seq(1,CCnt) ],
    Gw = getenv([Ta,Tb,1],Gateways),
    Fun = fun(X) ->
                  Node = getenv([Ta,Tb,X],Gateways),
                  Node ! {add,{[],Gw}},
                  Gw ! {add,{[Ta,Tb,X],Node}}
          end,
    [ Fun(X) || X <- lists:seq(2,CCnt) ],
    { [Ta,Tb], Gw }.

%% level C routing table
basic_routing_table([Ta,Tb,Tc],DCnt,Nodes) ->
    Gw = getenv([Ta,Tb,Tc,1],Nodes),
    Fun = fun(X) ->
                  Node = getenv([Ta,Tb,Tc,X],Nodes),
                  Node ! {add,{[],Gw}}, %% default gateway
                  Gw ! {add,{[Ta,Tb,Tc,X],Node}} %% gateway to each node
          end,
    [ Fun(X) || X <- lists:seq(2,DCnt)],
    {[Ta,Tb,Tc],Gw}.
