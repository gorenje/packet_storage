%%%-------------------------------------------------------------------
%%% File    : networkhelpers.erl
%%% Author  : Gerrit Riessen <gerrit.riessen@open-source-consultants.de>
%%% Description : network helper functions 
%%%
%%% Created : 31 Aug 2008 by Gerrit Riessen <gerrit.riessen@open-source-consultants.de>
%%%-------------------------------------------------------------------
-module(networkhelpers).

-export([ngetenv/3,rss_scrape/6,rss_link_data/3,rss_link_status/3,
         rss_test_net/0,nodes_network/0,rss_test_net_planetlab/0]).

-import(networkcreate,[adomain/4,adomain/1,adomain/5,adomain/2]).
-import(networkpacket,[packet_ruby/3,packet_netstore_request/4,packet_netstore/3,
                       packet_ruby_start/1,packet_ping/3,packet_ruby_scrape/3,
                       packet_ruby_seeder/5,packet_ruby_seeder/6,packet_multicast/3,
                       packet_getenv/4]).

ngetenv(Gw,To,Varname) ->
    ReplyTo = self(),
    Gw ! {packet, packet_getenv(100,To,ReplyTo,Varname)},
    receive
        {getenv_reply, ReplyTo, Value} ->
            Value
    after 2000 ->
            no_reply_from_node
    end.

nodes_network() ->
    AllNodes = nodes(),
    {GwDog, _N} = adomain([1], 2, length(AllNodes), 4),
    AllNodesWithDomains = assign_domains(AllNodes,2,[]),
    AllNets = [ {[1],GwDog} | lists:map( fun({Adomain,Node}) ->
                                                   {Gw,_N2} = adomain(Node, Adomain, 2, length(AllNodes), 4),
                                                   {Adomain, Gw} 
                                           end, AllNodesWithDomains )],
    [ G ! {add,{Domain,Gw}} || {Domain,Gw} <- AllNets,
                               {_D,G} <- lists:filter(fun({Dp,_Gp}) -> Dp =/= Domain end, AllNets) ],
    {GwDog, length(AllNodes)}.

assign_domains([],_,Result)             -> Result;
assign_domains([First|Rest],Cnt,Result) -> assign_domains(Rest,Cnt+1,[{[Cnt],First}|Result]).

rss_scrape(Gw,To,Start,Count,Scrapers,Collectors) ->
    Gw ! {packet, packet_ruby_start(To)},
    Gw ! {packet, packet_ruby_seeder(To,Start,Count, Scrapers, Collectors)},
    Gw.
rss_scrape(Gw,To,Start,Count,Scrapers,Collectors,UpdateCount) ->
    Gw ! {packet, packet_ruby_start(To)},
    Gw ! {packet, packet_ruby_seeder(To,Start,Count, Scrapers, Collectors,UpdateCount)},
    Gw.

rss_data_scraper(Gw,[]) -> Gw;
rss_data_scraper(Gw,[Scraper|Rest]) ->
    Gw ! {packet, packet_ruby_start(Scraper)},
    rss_data_scraper(Gw,Rest).
    
rss_link_data(Gw, _Collectors, []) -> Gw;
rss_link_data(Gw, Collectors, [Sink|Rest]) ->
    Gw ! {packet, packet_ruby_start(Sink)},
    [ Gw ! {packet, packet_netstore_request(20, Collector, Sink, fun({link,_}) -> true end)} || Collector <- Collectors ],
    rss_link_data(Gw, Collectors, Rest).
    
rss_link_status(Gw, _Collectors, []) -> Gw;
rss_link_status(Gw, Collectors, [Sink|Rest]) ->
    Gw ! {packet, packet_ruby_start(Sink)},
    [ Gw ! {packet, packet_netstore_request(20, Collector, Sink, fun({rss_link_status,_}) -> true end)} || Collector <- Collectors ],
    rss_link_status(Gw, Collectors, Rest).

rss_test_net_planetlab() ->
    {Gw, NumNodes} = nodes_network(),
    Cdm = lists:seq(1, NumNodes),
    Collectors  = [ [1,2,X,1] || X <- Cdm ],
    [ rss_link_data(   Gw, [ [1,2,X,1] ], [ [1,2,X,2] ] ) || X <- Cdm ],
    [ rss_link_status( Gw, [ [1,2,X,1] ], [ [1,2,X,3] ] ) || X <- Cdm ],
    Scrapers = [ [A,1,1,D] || A <- lists:seq(2,NumNodes+1), D <- [1,2] ],
    rss_data_scraper( Gw, Scrapers ),
    rss_scrape(Gw, [1,1,2,3], 2000, 1000, Scrapers, Collectors, 3 ).
    
    
rss_test_net() ->
    {Gw, _Nodes} = adomain([1],6,6,6),
    rss_scrape( rss_link_data( rss_link_status( rss_data_scraper( Gw, 
                                                                 [ [1,2,1,2], [1,2,2,2] ]),
                                                [ [1,1,2,1] ],
                                                [ [1,2,1,1] ]),
                               [ [1,1,3,1] ],
                               [ [1,2,2,1] ]),
                [1,4,4,4],
                10,
                10,
                [ [1,2,1,2], [1,2,2,2] ],
                [ [1,1,2,1], [1,1,3,1] ]).
