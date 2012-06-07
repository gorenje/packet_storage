%%%-------------------------------------------------------------------
%%% File    : networkexamples.erl
%%% Author  : Gerrit Riessen <gerrit.riessen@open-source-consultants.de>
%%% Description : some examples of using the network library
%%%
%%% Created : 29 Aug 2008 by Gerrit Riessen <gerrit.riessen@open-source-consultants.de>
%%%-------------------------------------------------------------------
-module(networkexamples).
-import(environmenthelpers, [getenv/2,setenv/3,delenv/2]).
-import(networkcreate,[adomain/4,adomain/1,adomain/5,adomain/2]).
-import(networkpacket,[packet_ruby/3,packet_netstore_request/4,packet_netstore/3,
                       packet_ruby_start/1,packet_ping/3,packet_ruby_scrape/3,
                       packet_ruby_seeder/5,packet_multicast/3]).
-export([ping_packet_from_ruby/0,data_from_ruby/0,netstore_and_retrieval/0,
        netstore_request/0,start_ruby_on_nodes/0,pass_unknown_packet_to_ruby/0,
        ruby_netstore_data/0,ruby_rss_scrape/0,scrape_packet/1,network/0,
        network_with_seeder/0,test_multicast/0,ruby_rss_scrape_two/0]).

test_multicast() ->
    {Gw,_} = adomain([127]),
    Gw ! {packet, packet_netstore_request(20,[127,2,1],[127,2,1,2], fun({link,_}) -> true end)},
    Gw.

network_with_seeder() ->
    {Gw,Collectors,Scrapers} = network(),
    Gw ! {packet, packet_ruby_start([130,3,4,1])},
    Gw ! {packet, packet_ruby_seeder([130,3,4,1], 0, 15000, Scrapers, Collectors)},
    Gw.

network() ->
    Nodes = ['ano@server1.org',
             'nym@server2.org',
             'ous@server3.org',
             'nam@server4.org',
             'eso@server5.org',
             'fse@rvers66.org'],
    [ net_adm:ping(N) || N <- Nodes ],
    [Hme,Bef,Dox,Osc,Hoe,Leh] = Nodes,
    {GwHme,_} = adomain(Bef,    [130],6,6,6),
    {GwDox,_} = adomain(Dox,    [128]),
    {GwBef,_} = adomain(Hme,    [127]),
    {GwOsc,_} = adomain(Osc,    [129]),
    {GwLeh,_} = adomain(Leh,    [131]),
    {GwHoe,_} = adomain(Hoe,    [132]),
    %% first start 4 ruby processes locally to store the data
    GwHme ! {packet, packet_ruby_start([130,1,1,1])},
    GwHme ! {packet, packet_ruby_start([130,1,2,1])},
    GwHme ! {packet, packet_ruby_start([130,1,3,1])},
    GwHme ! {packet, packet_ruby_start([130,1,4,1])},
    GwHme ! {packet, packet_ruby_start([130,1,5,1])},
    GwHme ! {packet, packet_netstore_request(20,[130,1,1,2],[130,1,1,1], fun({link,_}) -> true end)},
    GwHme ! {packet, packet_netstore_request(20,[130,1,1,2],[130,1,2,1], fun({link,_}) -> true end)},
    GwHme ! {packet, packet_netstore_request(20,[130,1,2,2],[130,1,3,1], fun({link,_}) -> true end)},
    GwHme ! {packet, packet_netstore_request(20,[130,1,2,2],[130,1,4,1], fun({link,_}) -> true end)},
    GwHme ! {packet, packet_netstore_request(20,[130,1,2,2],[130,1,5,1], fun({link,_}) -> true end)},
    %% since everybody has meet everyone, can send all packets to the home gateway
    GwHoe ! (GwLeh ! (GwBef ! (GwOsc ! (GwDox ! {add,{[130],GwHme}})))),
    GwHoe ! (GwLeh ! (GwBef ! (GwOsc ! (GwHme ! {add,{[128],GwDox}})))),
    GwHoe ! (GwLeh ! (GwBef ! (GwHme ! (GwDox ! {add,{[129],GwOsc}})))),
    GwHoe ! (GwLeh ! (GwHme ! (GwOsc ! (GwDox ! {add,{[127],GwBef}})))),
    GwHoe ! (GwHme ! (GwBef ! (GwOsc ! (GwDox ! {add,{[131],GwLeh}})))),
    GwHme ! (GwLeh ! (GwBef ! (GwOsc ! (GwDox ! {add,{[132],GwHoe}})))),
    %% start ruby processes on the remote networks
    %% start 4 processes on Dox and Beef, 2 on OSC
    Scrapers = [ [128,1,2,2], [128,1,1,2], [131,1,2,2], [132,1,1,2],
                 [129,2,2,1], [128,2,2,1], [131,1,1,2], [132,2,1,2],
                 [129,2,2,2], [128,2,2,2], [131,2,1,2], [132,2,2,2]
                ],
    [ GwHme ! {packet, packet_ruby_start(X)} || X <- Scrapers ],
    Collectors = [ [130,1,1,2], [130,1,2,2] ],
    {GwHme,Collectors,Scrapers}.

scrape_packet(Sub) ->
    setenv(type, ruby_rss_scrape,
    setenv(partners, [[Sub,2,2,2],[Sub,2,1,1],[Sub,1,2,2],[Sub,1,2,1]],
    setenv(src, "http://rubyforge.org/export/rss_sfnewreleases.php",
           packet_ping(20,[Sub,1,1,2],[0,0,0,0])))).

start_ruby_on_nodes() ->
    {Gw, _Nodes} = adomain([124]), %% two by two by two is the smallest network and the default
    Gw ! {packet, packet_ruby_start([124,1,2,2])},
    Gw ! {packet, packet_ruby_start([124,1,1,2])},
    Gw ! {packet, packet_ruby_start([124,2,1,2])},
    Gw ! {packet, packet_ruby_start([124,2,2,2])}.

pass_unknown_packet_to_ruby() ->
    {Gw, _Nodes} = adomain([124]), %% two by two by two is the smallest network and the default
    Gw ! {packet, packet_ruby_start([124,1,1,2])},
    Packet = setenv(type, ruby_netstore,
             setenv(partners, [ [124,2,2,2], [124,2,1,2],[124,1,2,2],[124,1,2,1] ],
             setenv(src, "User.find(1)",
                    packet_ping(10,[124,1,1,2],[0,0,0,0])))),
    Gw ! {packet, Packet},
    {Gw,Packet}.

ruby_rss_scrape_two() ->
    {Gw, _Nodes} = adomain([124]), %% two by two by two is the smallest network and the default
    Gw ! {packet, packet_ruby_start([124,2,1,2])}, %% this guy receives the data
    Gw ! {packet, packet_ruby_start([124,1,1,2])}, %% this guy generates the data
    %% create a data sink that receives anything that starts with "link"
    Gw ! {packet, packet_netstore_request(20,[124,2,1,1],[124,2,1,2], fun({link,_}) -> true end)},
    Packet = setenv(type, ruby_rss_scrape,
             setenv(partners, [ [124,2,2,2],[124,2,1,1] ],
             setenv(src, "http://rubyforge.org/export/rss_sfnewreleases.php",
                    packet_ping(20,[124,1,1,2],[0,0,0,0])))),
    Gw ! {packet,Packet}.

ruby_rss_scrape() ->
    {Gw, _Nodes} = adomain([124]), %% two by two by two is the smallest network and the default
    Gw ! {packet, packet_ruby_start([124,2,1,2])}, %% this guy receives the data
    Gw ! {packet, packet_ruby_start([124,1,1,2])}, %% this guy generates the data
    %% create a data sink that receives anything that starts with "link"
    Gw ! {packet, packet_netstore_request(20,[124,2,1],[124,2,1,2], fun({link,_}) -> true end)},
    Packet = setenv(type, ruby_rss_scrape,
             setenv(partners, [[124,2,2,2],[124,2,1,1],[124,1,2,2],[124,1,2,1]],
             setenv(src, "http://rubyforge.org/export/rss_sfnewreleases.php",
                    packet_ping(20,[124,1,1,2],[0,0,0,0])))),
    Gw ! {packet,Packet}.

ruby_netstore_data() ->
    {Gw, _Nodes} = adomain([124]), %% two by two by two is the smallest network and the default
    Gw ! {packet, packet_ruby_start([124,2,1,2])}, %% this guy receives the data
    Gw ! {packet, packet_ruby_start([124,1,1,2])}, %% this guy generates the data
    Gw ! {packet, packet_netstore_request(20,[124,2,1],[124,2,1,2],{<<"User.find(1)">>})},
    Packet = setenv(type, ruby_netstore,
             setenv(partners, [[124,2,2,2],[124,2,1,1],[124,1,2,2],[124,1,2,1]],
             setenv(src, "User.find(1)",
                    packet_ping(20,[124,1,1,2],[0,0,0,0])))),
    Gw ! {packet,Packet}.

%% sending a ping packet from a ruby process
ping_packet_from_ruby() ->
    {Gw, _Nodes} = adomain([124],4,4,4),
    Gw ! {packet, packet_ruby([124,1,2,2],[124,2,1,1],"f.send!(:ruby_results,packet_ping(12,[124,1,2,2],[124,2,1,1])) ; packet_ping(12,[124,1,2,2],[124,1,1,1])")}.

%% get some data from a ruby process. this use active record to retrieve the data from a DB
data_from_ruby() ->
    {Gw, _Nodes} = adomain([124],4,4,4),
    Gw ! {packet, packet_ruby([124,1,2,2],[124,2,1,1],"User.find(2).to_yaml")}.

%% request for network storage
netstore_request() ->
    {Gw, _Nodes} = adomain([127],4,4,4),
    Gw ! {packet, packet_netstore_request(20,[127,1,4],[127,2,2,2],{one,1})}.

%% simple example of network storage and retrieval
netstore_and_retrieval() ->
    {Gw, _Nodes} = adomain([127],4,4,4),
    NetstorePacket = packet_netstore(20, [ [127,1,2,1], [127,1,3,1], [127,1,4,1], [127,2,1,1]], {{one,1},{this,is,the,data,packet}}),
    Gw ! {packet, packet_netstore_request(20,[127,2,1],[127,2,2,2],{one,1})},
    Gw ! {packet, NetstorePacket},
    Gw ! {packet, NetstorePacket}. %% this will not resend the packet to the original requester

