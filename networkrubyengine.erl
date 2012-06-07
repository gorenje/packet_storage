%%%-------------------------------------------------------------------
%%% File    : networkrubyengine.erl
%%% Author  : Gerrit Riessen <gerrit.riessen@open-source-consultants.de>
%%% Description : wrapper for starting a ruby engine process
%%%
%%% Created : 24 Aug 2008 by Gerrit Riessen <gerrit.riessen@open-source-consultants.de>
%%%-------------------------------------------------------------------
-module(networkrubyengine).
-import(environmenthelpers, [getenv/2,setenv/3,delenv/2]).
-export([ruby_start/1]).

%% used to started the ruby process by the parent node. The architecture here is:
%%   Parent Node with IP ---> This ---> Ruby process
%% the parent node handles passing around packets in the network and passes any
%% packets it doesn't known about (but addressed to it) to us. We then pass those
%% packets to the ruby process hoping it won't die. If it does die, we respawn a 
%% new process.
%% The parent process is also responsible for not respawning this process, i.e. it
%% prevents itself from spawning endless number of ruby processes. The rules are: each
%% node has exactly one or zero ruby processes.
%% If you want multiple rubys (and let's face it: who doesn't!), then create multiple
%% nodes and send them each a ruby_start packet.

%% used to restart the ruby process if it dies
restart(Name) ->
    Env = [ {"NODE_NAME", Name} ],
    Cmd = "ruby ./networkrubyengine.rb",
    Port = open_port({spawn, Cmd}, [{packet, 4}, {env, Env}, binary, use_stdio,exit_status]),
    {Port,[],Name}.

% @spec Name is a string!
ruby_start(Name) ->
    spawn(fun()-> 
              process_flag(trap_exit,true),
              {Port,Packet,Newname} = restart(Name),
              port_loop(Port,Packet,Newname)
          end).

reboot_ruby(Packet,Reason,Name) ->
    Pid = getenv(from_pid,Packet),
    if 
        is_pid(Pid) -> send_ruby_died(Pid,Packet,Reason);
        true -> []
    end,
    restart(Name).
    
send_ruby_died(Pid,Packet,Reason)  ->
    case is_process_alive(Pid) of
        true ->
            Pid ! {packet,setenv(admin_type,ruby_exiting,
                                 setenv(ruby_results,io_lib:format("Died. Reason: ~w, restarting...",[Reason]),
                                        Packet))};
        _ -> []
    end.
    
port_loop(Port,Packet,Name) ->
    receive 
        %% the next two handle the death-of-a-ruby-process. basically just respawn a new
        %% ruby process for handling packets. unfortunately the state of the ruby process
        %% is lost, but perhaps one should write more stable ruby code!
        {'EXIT',Port,Reason} ->
            {NewPort,NewPacket,NewName} = reboot_ruby(Packet,Reason,Name),
            port_loop(NewPort,NewPacket,NewName);

        {Port, {exit_status, Status}} ->
            {NewPort,NewPacket,NewName} = reboot_ruby(Packet,Status,Name),
            port_loop(NewPort,NewPacket,NewName);

        %% this is an unknown packet from another node to be passed onto the ruby process
        %% the ruby process either does something with it or it gets ignored
        {packet,Type,NewPacket} ->
            CmdData = term_to_binary({packet, Type, NewPacket}),
            Port ! {self(), {command, CmdData}},
            port_loop(Port, setenv(type, admin,
                            setenv(admin_type, ruby_results, NewPacket)),Name);

        %% this is kinda old code, it's similar to the above but different. this should
        %% be removed in the long term.
        {packet,NewPacket} ->
            CmdData = term_to_binary({execute, term_to_binary(getenv(ruby_cmd,NewPacket))}),
            Port ! {self(), {command, CmdData}},
            port_loop(Port,NewPacket,Name);

        %% this handles any responses from the ruby process. they always have this
        %% form and need to be "deserialized"
        {Port, {data, Data}} ->
            {Cmd, PacketData} = binary_to_term(Data),
            {NewPort,NewPacket,NewName} = case Cmd of
                ok ->
                    {Port,Packet,Name};
                bad ->
                    Parent = getenv(from_pid,Packet),
                    case is_process_alive(Parent) of 
                        true -> Parent ! {packet,setenv(admin_type,ruby_failed,
                                                 setenv(ruby_results,PacketData,Packet))};
                        _ -> []
                    end,
                    {Port,Packet,Name};
                ruby_results ->
                    Parent = getenv(from_pid,Packet),
                    case is_process_alive(Parent) of 
                       true -> Parent ! {packet,setenv(ruby_results,PacketData,Packet)};
                        _ -> []
                    end,
                    {Port,Packet,Name};
                _ ->
                    io:format("unknown match in port_loop~n",[]),
                    {Port,Packet,Name}
            end,
            port_loop(NewPort,NewPacket,NewName);

        Any ->
            io:format("Got following unknown: ~w~n", [Any]),
            port_loop(Port,Packet,Name)
    end.
