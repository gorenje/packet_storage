%%%-------------------------------------------------------------------
%%% File    : environmenthelpers.erl
%%% Author  : Gerrit Riessen <gerrit.riessen@open-source-consultants.de>
%%% Description : set,get,del environment helpers
%%%
%%% Created : 22 Aug 2008 by Gerrit Riessen <gerrit.riessen@open-source-consultants.de>
%%%-------------------------------------------------------------------
-module(environmenthelpers).
-export([getenv/2,setenv/3,delenv/2]).

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
