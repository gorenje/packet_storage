%%%-------------------------------------------------------------------
%%% File    : networkpacket.erl
%%% Author  : Gerrit Riessen <gerrit.riessen@open-source-consultants.de>
%%% Description : helpers for creating different types of packets
%%%
%%% Created : 24 Aug 2008 by Gerrit Riessen <gerrit.riessen@open-source-consultants.de>
%%%-------------------------------------------------------------------
-module(networkpacket).
-export([packet_ping/3, packet_admin/4, packet_ruby/3, 
         packet_netstore/3,packet_multicast/2,packet_multicast/3,
         packet_netstore_request/4,packet_ruby_start/1,packet_ruby_scrape/3,
         packet_ruby_seeder/5,packet_ruby_seeder/6,packet_getenv/4]).

-import(environmenthelpers, [getenv/2,setenv/3,delenv/2]).

%% helper for creating a packet
packet_ping(Ttl, To, From) ->
    setenv(ttl,  Ttl,
    setenv(to,   To,
    setenv(from, From,
    setenv(type, ping, [])))).

packet_admin(Ttl,To,From,AdminType) ->
    setenv(type,admin,
    setenv(admin_type, AdminType, 
           packet_ping(Ttl,To,From))).

packet_getenv(Ttl, To, From, Varname) ->
    setenv(varname,Varname,
    setenv(replyto,From, %% in this case the from is not an IP but a pid
           packet_admin(Ttl,To,[0,0,0,0],getenv))).

packet_multicast(Ttl, From) ->
    packet_multicast(Ttl, [], From).

packet_multicast(Ttl, To, From) ->
    setenv(type, multicast, 
    setenv(mc_type,alive,
           packet_ping(Ttl,To,From))).

packet_netstore_request(Ttl,To,From,DataId) ->
    setenv(data_id, DataId,
    setenv(mc_type,netstore_request,
           packet_multicast(Ttl,To,From))).

packet_netstore(Ttl,[To,From|Rest],{DataId,Data}) ->
    setenv(type,     netstore,
    setenv(data_id,  DataId,
    setenv(data,     Data,
    setenv(partners, Rest ++ [From,To],
           packet_ping(Ttl,To,From))))).

packet_ruby(To,From,Cmd) ->
    setenv(ruby_cmd,Cmd,
           packet_admin(100,To,From,ruby)).

packet_ruby_start(To) ->
    packet_admin(100,To,[0,0,0,0],ruby_start).

packet_ruby_scrape(To,Url,Partners) ->
    setenv(type, ruby_rss_scrape, 
    setenv(partners, Partners, 
    setenv(src, Url,
           packet_ping(40,To,[0,0,0,0])))).

%% Sources are the scraper nodes and Partners are the collector nodes
packet_ruby_seeder(To,StartAt,Cnt,Sources,Partners) ->
    packet_ruby_seeder(To,StartAt,Cnt,Sources,Partners,1).
packet_ruby_seeder(To,StartAt,Cnt,Sources,Partners,UpdateCnt) ->
    setenv(type, ruby_rss_seeder, 
    setenv(sources, Sources, 
    setenv(start_at, StartAt,
    setenv(count, Cnt,
    setenv(partners, Partners,
    setenv(update_count, UpdateCnt,
           packet_ping(40,To,[0,0,0,0]))))))).
    
