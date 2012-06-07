%%%-------------------------------------------------------------------
%%% File    : randomnn.erl
%%% Author  : Gerrit Riessen <gerrit.riessen@open-source-consultants.de>
%%% Description : simple randome generator based on a neural network
%%%
%%% Created : 15 Aug 2008 by Gerrit Riessen <gerrit.riessen@open-source-consultants.de>
%%%-------------------------------------------------------------------
-module(randomnn).
-compile(export_all).
%% -export([start/0,start/1,get_num/1]).

%% start a NN with 5 nodes which randomly fire 
%% use start() to create the nodes and get_num(Nodes) to
%% a random value:
%%   Nodes = randomnn:start().
%%   randomnn:get_num(Nodes).
%% for more nodes than five, use start(Cnt).

%% environment handlers, should be separate library (but they probably exist already)
delenv(_What,[],NewEnv)                 -> NewEnv;
delenv(What,[{What,_Val}|Rest],NewEnv)  -> lists:flatten([Rest|NewEnv]);
delenv(What,[{Name,Val}|Rest],NewEnv)   -> delenv(What,Rest,[{Name,Val}|NewEnv]).
delenv(What,Env)                        -> delenv(What,Env,[]).
getenv(_What,[])                        -> 0;
getenv(What,[{What,Val}|_Rest])         -> Val;
getenv(What,[{_,_}|Rest])               -> getenv(What,Rest).
setenv(What,Val,Env,_)                  -> [{What,Val}|Env].
setenv(What,Val,Env)                    -> setenv(What,Val,delenv(What,Env),dontcare).


config_all([],Nodes) ->
    Nodes;
config_all([Head|Rest],Nodes) ->
    Head ! {config,lists:delete(Head,Nodes),initial_env()},
    config_all(Rest,Nodes).
config_all(Nodes) ->
    config_all(Nodes,Nodes).

get_num([],Sum) ->
    Sum;
get_num([Head|Rest],Sum) ->
    Head ! {num,self()},
    Num = receive
              {num,N} ->
                  N
          after 1000 ->
                  0
          end,
    get_num(Rest,Sum + Num).
get_num(Nodes) ->
    entropy(Nodes),
    get_num(Nodes,0).

entropy([]) ->
    [];
entropy([Head|Nodes]) ->
    Head ! {fire},
    entropy(Nodes).

start() ->
    config_all(new_nodes(5)).
start(Cnt) ->
    config_all(new_nodes(Cnt)).

initial_env() ->
    Env2 = setenv(fireinterval,random:uniform(500) + 10,[]),
    setenv(firecounter,random:uniform(32) + 5,Env2).

new_node() ->
    spawn(randomnn, init, [self()]).

init(_From) ->
    loop().

new_nodes(0, Nodes) ->
    Nodes;
new_nodes(Cnt, Nodes) ->
    new_nodes(Cnt-1,[new_node()|Nodes]).
new_nodes(Cnt) ->
    new_nodes(Cnt,[]).


fire_funct(Env) ->
    FireCnt = getenv(firecounter,Env),
    setenv(exitcode,FireCnt-1,Env).

fire([]) ->
    done;
fire([Node|Rest]) ->
    Node ! {fire},
    fire(Rest).

got_fire_send_fire(1,Nodes)        -> fire(Nodes);
got_fire_send_fire(_RetVal,_Nodes) -> [].
got_fire_call_func(0,Env)          -> 
    fire_funct(Env);
got_fire_call_func(Func,Env)       -> 
    Func(Env).

got_fire(Nodes,Env) ->
    Env2 = got_fire_call_func(getenv(funct,Env),Env),
    got_fire_send_fire(getenv(exitcode,Env2),Nodes),
    Env2.

%% main loop. wait for fire messages from other nodes
%% each time we execute our fire function and see whether
%% we in turn also fire.
loop(Nodes,Env) ->
    Time = getenv(fireinterval,Env),
    receive
        {num,From} ->
            From ! {num,getenv(firecounter,Env)},
            loop(Nodes,Env);
        {fire} ->
            loop(Nodes,got_fire(Nodes,Env));
        Any ->
            io:format("~w Main Loop got unknown ~w ~n",[self(),Any]),
            loop(Nodes,Env)
    after Time ->
            fire(Nodes),
            loop(Nodes,initial_env())
    end.
    

%% wait for our nodes and environment
loop() ->
    receive
        {config,Nodes,Env} ->
            loop(Nodes,Env);
        Any ->
            io:format("~w Config Loop got unknown ~w ~n",[self(),Any]),
            loop()
    end.
            
