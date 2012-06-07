%%%-------------------------------------------------------------------
%%% File    : neuralnetwork.erl
%%% Author  : Gerrit Riessen <gerrit.riessen@open-source-consultants.de>
%%% Description : simple neural network
%%%
%%% Created : 15 Aug 2008 by Gerrit Riessen <gerrit.riessen@open-source-consultants.de>
%%%-------------------------------------------------------------------
-module(neuralnetwork).
-compile(export_all).
%% -export([start/0,init/1]).

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


config_all([],Nodes)          ->
    Nodes;
config_all([Head|Rest],Nodes) ->
    Head ! {config,lists:delete(Head,Nodes),initial_env()},
    config_all(Rest,Nodes).
config_all(Nodes)             ->
    config_all(Nodes,Nodes).

config_output([],Nodes)       ->
    Nodes;
config_output([Head|Rest],Nodes) ->
    Env = initial_env(),
    Head ! {config,[],setenv(funct,fun() -> fire_funct_output(Env) end},
    config_output(Rest,Nodes).
config_output(Nodes)             ->
    config_output(Nodes,Nodes).


create_nodes()    ->
    create_nodes(5).
create_nodes(Cnt) ->
    config_all(new_nodes(Cnt)).
create_nodes(output,Cnt) ->
    config_output(new_nodes(Cnt)),


initial_env() ->
    setenv(funct,       fun(Env) -> fire_funct(Env) end,
    setenv(exitcode,    0,
    setenv(firecounter, random:uniform(32),
           []))).

new_node() ->
    spawn(neuralnetwork, init, [self()]).

new_nodes(0, Nodes)   ->
    Nodes;
new_nodes(Cnt, Nodes) ->
    new_nodes(Cnt-1,[new_node()|Nodes]).
new_nodes(Cnt)        ->
    new_nodes(Cnt,[]).

init(_From) ->
    loop().

%% this should always return a new environment list
%% with the value of exitcode specifying whether we
%% should fire.
fire_funct(Env) ->
    FireCnt = getenv(firecounter,Env),
    setenv(exitcode,FireCnt-1,Env).

fire_funct_output(Env) ->
    io:format("Node ~w (~w) got fired",[getenv(name,Env),self()]),
    setenv(exitcode,0,Env).

fire([])          ->
    done;
fire([Node|Rest]) ->
    Node ! {fire},
    fire(Rest).

got_fire_send_fire(1,Nodes)        -> fire(Nodes);
got_fire_send_fire(_RetVal,_Nodes) -> [].

got_fire(Nodes,Env) ->
    Env2 = (getenv(funct,Env))(Env),
    got_fire_send_fire(getenv(exitcode,Env2),Nodes),
    Env2.

%% main loop. wait for fire messages from other nodes
%% each time we execute our fire function and see whether
%% we in turn also fire.
loop(Nodes,Env) ->
    receive
        {funct,Funct} ->
            loop(Nodes,setenv(funct,Funct,Env));
        {add,From} ->
            loop([From|Nodes],Env);
        {del,From} ->
            loop(lists:delete(From,Nodes),Env);
        {set,Key,Val} ->
            loop(Nodes,setenv(Key,Val,Env));
        {getenv,From} ->
            From ! {env,self(),Env},
            loop(Nodes,Env);
        {setenv,NewEnv} ->
            loop(Nodes,NewEnv);
        {fire} ->
            loop(Nodes,got_fire(Nodes,Env));
        Any ->
            io:format("~w Received unknown msg ~w~n",[self(),Any]),
            loop(Nodes,Env)
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

