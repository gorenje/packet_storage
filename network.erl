%%%-------------------------------------------------------------------
%%% File    : network.erl
%%% Author  : Gerrit Riessen <gerrit.riessen@open-source-consultants.de>
%%% Description : network
%%%
%%% Created : 15 Aug 2008 by Gerrit Riessen <gerrit.riessen@open-source-consultants.de>
%%%-------------------------------------------------------------------
-module(network).

-compile(export_all).
%-export([new_node/2,new_node/3]).

-import(lists,[map/2]).
-import(environmenthelpers, [getenv/2,setenv/3,delenv/2]).
-import(networkroutingtable,[addr_to_node/2,add_route/2,all_routes/1,domain_match/2]).
-import(networkrubyengine,[ruby_start/1]).
-import(networkcreate,[adomain/4]).
-import(networkpacket,[packet_ruby/3,packet_ping/3]).

%% This is it: core of the entire network. A node with an IP and the intelligence
%% to route packets to accoding to it's routing table. It can also spawn a ruby
%% process and pass it all packets that are addressed for it but it doesn't know
%% about. 

%% Network-embedded storage:
%%  - data is sent out to the network and is ping'ed in a circle in 
%%  - query is sent to the network and everybody that has the data sends it to the queryee
%%    if the queryee gets the data multiple times, it gets ignored.
%%  - all nodes store query but only those that have the data send it to the queryee
%%  - queries store are fifo and have a max so that nodes don't get flooded with queries
%%  - each data ping has a ttl and if that expires, the data is dropped. 
%%    prevents permemant pings and stale data
%% intention is to have the data local to the queryee and save requests to the database node

%% network should be created with a convention,
%%  e.g. 127.1.1.X are daten
%%       127.1.2.X are services
%%       127.1.3.X are monitors
%%       127.1.4.X are ????
%%    etc
%% a-level domain should be ( i.e. 127/24, 128/24,etc) physically different machines

%% TODO
%% An improvement to the routing could be that a gateway keeps a list of
%% of the most popular routes and then provides the node involved direct
%% routes. I.e. sends an {add,Route,Node} to the appropriate node.
%% This would be done every few seconds/minutes. However, it must also be
%% ensured the routing table doesn't get too big.

%% -export([start/0,init/1]).
-ifdef(debug).
log(Str,Paras) ->
    io:format( string:concat("~w ",Str), [self()|Paras]),
    [].
-else.
log(_Str,_Paras) ->
    [].
-endif.

init(_From) -> 
    process_flag(trap_exit,true),
    loop().

initial_env(Name,Addr) ->
    setenv(address, Addr,
    setenv(name, Name, 
    setenv(ns_req_queue, [],
    setenv(routingtable, [],
    setenv(nodename, set_in_loop,
    setenv(nodepid, set_in_loop,
           [])))))).

%% this is the only thing that is need to create a network.
%% see networkcreate.erl for how it is used.
new_node(Name,Addr) ->
    new_node(node(),Name,Addr).
new_node(RemoteNode,Name,Addr) ->
    Node = spawn(RemoteNode, network, init, [self()]),
    Node ! {config,initial_env(Name,Addr)},
    Node.

%%
%% called to handle various admin packets
%%
handle_admin_packet(ruby_exiting,Packet,RoutingTable,Env) ->
    log("Got ruby exiting ~w~n",[getenv(ruby_results,Packet)]),
    {RoutingTable,Env};
handle_admin_packet(ruby_failed,Packet,RoutingTable,Env) ->
    log("Got ruby failed ~w~n",[getenv(ruby_results,Packet)]),
    {RoutingTable,Env};
handle_admin_packet(ruby_results,Packet,RoutingTable,Env) ->
    RubyResults = getenv(ruby_results,Packet),
    log("(new) Got ruby results ~p~n",[RubyResults]),
    case RubyResults of
        {packet,Pkt} -> 
            log("found packet (one): ~p~n",[Pkt]),
            route_packet(Pkt, getenv(routingtable,Env));
        _ -> []
    end,
    {RoutingTable,Env};

handle_admin_packet(ruby_start,_Packet,RoutingTable,Env) ->
    Name = getenv(name,Env),
    case whereis(list_to_atom(Name)) of
        undefined -> register(list_to_atom(Name), ruby_start(Name));
        _ -> []
    end,
    {RoutingTable,Env};

handle_admin_packet(ruby,Packet,RoutingTable,Env) ->
    handle_admin_packet(ruby_start,Packet,RoutingTable,Env),
    P = setenv(to,         getenv(from,Packet),
        setenv(from,       getenv(to,Packet),
        setenv(type,       admin,
        setenv(admin_type, ruby_results,
        setenv(from_pid,   self(), Packet))))),
    list_to_atom(getenv(name,Env)) ! {packet,P},
    {RoutingTable,Env};

handle_admin_packet(getenv,Packet,RoutingTable,Env) ->
    ReplyTo = getenv(replyto,Packet),
    ReplyTo ! {getenv_reply,ReplyTo,getenv(getenv(varname,Packet),Env)},
    {RoutingTable,Env};

handle_admin_packet(_Type,_Packet,RoutingTable,Env) ->
    {RoutingTable,Env}.


%%
%% handle various multicast packets
%%
handle_multicast_packet(netstore_request,Packet,RoutingTable,Env) ->
    NewReqQueue = case domain_match(getenv(to,Packet),getenv(address,Env)) of
                      true -> 
                          log( "~w: Got Netstore Request for: ~p~n", 
                               [getenv(address,Env), getenv(data_id,Packet)]),
                          lists:flatten([ {getenv(data_id, Packet), getenv(from, Packet)},
                                          lists:sublist(getenv(ns_req_queue,Env),19)]);
                      _ -> getenv(ns_req_queue,Env)
                  end,
    {RoutingTable,setenv(ns_req_queue,NewReqQueue,Env)};

handle_multicast_packet(_MCType,_Packet,RoutingTable,Env) ->
    {RoutingTable,Env}.

%% 
%% helpers for routing a packet on
%%

%% multicast are routed to all domains and nodes in our routing table
%% except for default gateways
route_multicast(Packet,RoutingTable) ->
    P = setenv(ttl, getenv(ttl,Packet) - 1, Packet),
    To = getenv(to, Packet),
    Funct = fun({Route,Node}) ->
                    case domain_match(To,Route) of
                        true -> 
                            log("########## Routing multicast to ~w~n",[Route]),
                            Node ! {packet,P};
                        _ -> []
                    end
            end,
    map(Funct, all_routes(RoutingTable)).


%% we got a netstore packet that was maybe requested by another node
route_netstore(_Packet, _RoutingTable, _DataId, []) -> 
    log("******** Routing netstore stopped~n",[]),
    [];
route_netstore(Packet, RoutingTable, DataId, [{DataIdFun,To}|Rest]) when is_function(DataIdFun) ->
    log("******** Funct Routing netstore~n",[]),
    try DataIdFun(DataId) of
        true -> 
            log("******** Funct Routing netstore data to ~w~n",[To]),
            route_packet( setenv(ttl,100,
                              setenv(to,To,
                              setenv(type,netstore_data,
                                     Packet))), RoutingTable)
    catch
        _:_ -> []
    end,
    route_netstore(Packet, RoutingTable, DataId, Rest);
route_netstore(Packet, RoutingTable, DataId, [{DataId,To}|Rest]) ->
    log("******* Routing netstore data to ~w~n",[To]),
    route_packet( setenv(ttl,100,
                  setenv(to,To,
                  setenv(type,netstore_data,
                         Packet))), RoutingTable),
    route_netstore(Packet, RoutingTable, DataId, Rest);
route_netstore(Packet, RoutingTable, DataId, [_Head|Rest]) ->
    route_netstore(Packet, RoutingTable, DataId, Rest).

%% send on a packet
route_packet(Packet,RoutingTable) ->
    Ttl = getenv(ttl,Packet),
    To = getenv(to,Packet),
    Node = addr_to_node(To,RoutingTable),
    log("routing ~w to ~w~n",[To,Node]),
    Node ! {packet,setenv(ttl,Ttl-1,Packet)}.

%% wait for our initial nodes and environment
loop() ->
    receive
        {config,Env} ->
            log("Address ~w~n",[getenv(address,Env)]),
            loop(getenv(routingtable,Env), setenv(nodepid, self(), 
                                           setenv(nodename,node(),Env)));
        Any ->
            log("~w Config Loop got unknown ~w ~n",[self(),Any]),
            loop()
    end.

%% take no prisoners, ttl zero --> packet is dropped to the floor
handle_packet(_Type, _To, _MyAddress, _From, 0, Packet, RoutingTable,Env) ->
    log("Ttl zero for ~w, dropping packet~n",[Packet]),
    {RoutingTable,Env};

%% well i'll be damn'ed, an admin packet!
handle_packet(admin,MyAddress,MyAddress,_From,_Ttl,Packet,RoutingTable,Env) ->
    handle_admin_packet(getenv(admin_type,Packet),Packet,RoutingTable,Env);

%% multicast
handle_packet(multicast,_To,_MyAddress,From,_Ttl,Packet,RoutingTable,Env) ->
    log("Multicast from ~w~n",[From]),
    route_multicast(Packet,RoutingTable), %% multicast packets are always propagated
    handle_multicast_packet(getenv(mc_type,Packet), Packet, RoutingTable, Env);

handle_packet(pong,MyAddress,MyAddress,From,_Ttl,_Packet,RoutingTable,Env) ->
    log("Pong! From ~w~n",[From]),
    {RoutingTable,Env};
    
handle_packet(ping,MyAddress,MyAddress,From,_Ttl,Packet,RoutingTable,Env) ->
    log("Ping! From ~w~n",[From]),
    route_packet(setenv(type,pong,
                 setenv(from,MyAddress,
                 setenv(to,From,Packet))), RoutingTable),
    {RoutingTable,Env};

handle_packet(netstore,MyAddress,MyAddress,From,_Ttl,Packet,RoutingTable,Env) ->
    log("Netstore (~w) from ~w with Id ~w~n",[MyAddress,From,getenv(data_id,Packet)]),
    [Next|Rest] = getenv(partners,Packet),
    %% TODO check whether we have a request for this data and route packet to the requestee
    %% ensure that the Ttl is high enough to reach the recepient. although if it isn't then
    %% it's a sign for that the data isn't close enough to the requestee. In which case the
    %% requestee will go straight for the database and the database will send out a new netstore
    %% for that neck of the woods. I.e. the data is stored within the vicinity of the requesting
    %% node for future requests.
    route_packet(setenv(type,      netstore,
                 setenv(from,      MyAddress,
                 setenv(to,        Next,
                 setenv(partners,  Rest ++ [Next],
                        Packet)))), RoutingTable),
    route_netstore(setenv(from,MyAddress,Packet), RoutingTable, getenv(data_id,Packet), getenv(ns_req_queue,Env)),
    NewReqQueue = lists:filter(fun({DataId,_}) -> DataId =/= getenv(data_id,Packet) end, getenv(ns_req_queue,Env)),
    {RoutingTable,setenv(ns_req_queue,NewReqQueue,Env)};

%% Used to pass certain and unknown packets to the subprocess (if any).
%% So far the following known-unknowns are route to the subprocess from here:
%%   netstore_data - answer on a netstore request. this is not the netstore packet 
%%                   which is pinged between nodes and contains the data.
%%   ruby_* - various ruby specific packets (except ruby_start)
handle_packet(Type,MyAddress,MyAddress,From,_Ttl,Packet,RoutingTable,Env) ->
    log("Unknown packet (Type: ~p) for me (~w) from ~w: ~w~n",[Type,MyAddress,From,Packet]),
    %% check whether this node has a sub-process registered, if so
    %% pass the packet on to it.
    Name = getenv(name,Env),
    case whereis(list_to_atom(Name)) of
        undefined -> [];
        _ -> list_to_atom(Name) ! {packet, getenv(type,Packet), setenv(from_pid,self(),Packet)}
    end,
    {RoutingTable,Env};

%% packet wasn't for us, route it on
handle_packet(_Type,_To,_MyAddress,_From,_Ttl,Packet,RoutingTable,Env) ->
    route_packet(Packet,RoutingTable),
    {RoutingTable,Env}.

%% main loop. wait for fire messages from other nodes
%% each time we execute our fire function and see whether
%% we in turn also fire.
loop(RoutingTable,EnvOrig) ->
    Env = setenv(pid,self(),
          setenv(routingtable,RoutingTable,
                 EnvOrig)),
    MyAddress = getenv(address,Env),
    receive
        %% admin functionality to config the node
        %% routing table manipulation
        {add,{Route,Gateway}} ->
            log("~w: adding route ~w to ~w~n",[MyAddress,Route,Gateway]),
            loop(add_route({Route,Gateway},RoutingTable),Env);
        {add,Gateway} ->
            log("adding route getting addr ~w~n",[Gateway]),
            Gateway ! {get,address,self()},
            Addr = receive 
                       {address,Value,Gateway} ->
                           log("received addr ~w from ~w~n",[Value,Gateway]),
                           Value
                   end,
            self() ! {add,{Addr,Gateway}},
            loop(RoutingTable,Env);
        {del,Route} ->
            loop(lists:delete(Route,RoutingTable),Env);

        %% environment manipulation
        {set,Key,Val} ->
            loop(RoutingTable,setenv(Key,Val,Env));
        {get,Key,From} ->
            From ! {Key,getenv(Key,Env),self()},
            log("got get for ~w from ~w val ~w~n",[Key,From,getenv(Key,Env)]),
            loop(RoutingTable,Env);
        {getenv,From} ->
            From ! {env,self(),Env},
            loop(RoutingTable,Env);
        {setenv,NewEnv} ->
            loop(RoutingTable,NewEnv);

        %% split up the packet and resend it to ourselve
        %% this allows us to pattern match on the contents of the pattern
        %% actually this is done better with a function but this is the first
        %% version and it works. 
        {packet,Packet} ->
            {NewRoutingTable,NewEnv} = handle_packet(getenv(type,Packet), getenv(to,Packet), 
                                                     MyAddress, getenv(from,Packet), getenv(ttl,Packet), 
                                                     Packet, RoutingTable, Env),
            loop(NewRoutingTable,NewEnv);

        %% Debug in case we get an unexpected message
        Any ->
            io:format("~w Received unknown msg ~w~n",[self(),Any]),
            loop(RoutingTable,Env)
    end.

%%
%% code for the example
%%
example() ->
    Node = simple_network(),
    Packet = packet_ping(12,[127,0,1,5],[127,0,0,6]),
    send_packet(Node,Packet).

send_packet(Node,Packet) ->
    N = new_node("Sender",getenv(from,Packet)),
    Node ! {add,N},
    Node ! {packet,Packet},
    receive
    after 5000 ->
            log("Exiting send packet~n",[])
    end,
    Node ! {del,N}.

register_nomatchingnode(Node,undefined) ->
    register(nomatchingnode,Node);
register_nomatchingnode(Node,_)         ->
    unregister(nomatchingnode),
    register_nomatchingnode(Node,undefined).
register_nomatchingnode(Node)           ->
    register_nomatchingnode(Node,whereis(nomatchingnode)).

simple_network() ->
    N0 = new_node("DeathStar",[0,0,0,0]),
    register_nomatchingnode(N0),
    N1 = new_node("Node01",[127,0,0,1]),
    N2 = new_node("Node02",[127,0,0,2]),
    N3 = new_node("Node03",[127,0,0,3]),
    N4 = new_node("Node04",[127,0,0,4]),
    N5 = new_node("Node05",[127,0,0,5]),
    N11 = new_node("Node06",[127,0,1,1]),
    N12 = new_node("Node07",[127,0,1,2]),
    N13 = new_node("Node08",[127,0,1,3]),
    N14 = new_node("Node09",[127,0,1,4]),
    N15 = new_node("Node10",[127,0,1,5]),
    %% construct network
    N1 ! {add,N2},
    N1 ! {add,N3},
    N1 ! {add,N4},
    N1 ! {add,N5},
    N1 ! {add,{[127,0,1],N5}},
    %% default gateway
    N2 ! {add,{[],N1}},
    N3 ! {add,{[],N1}},
    N4 ! {add,{[],N1}},
    N5 ! {add,{[],N1}},
    N5 ! {add,{[127,0,0],N1}},
    N5 ! {add,{[127,0,1],N11}},
    N5 ! {add,N11},
    %% 127.0.1 network
    N11 ! {add,N12},
    N11 ! {add,N13},
    N11 ! {add,N14},
    N11 ! {add,N15},
    N11 ! {add,{[127,0,0],N5}},
    N12 ! {add,{[],N11}},
    N13 ! {add,{[],N11}},
    N14 ! {add,{[],N11}},
    N15 ! {add,{[],N11}},
    N1.
