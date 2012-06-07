-module(server). 
-export([start/3, rpc/2, swap_code/2,load_code/2,ping/1]). 

start(Node, Name, Mod) -> 
    spawn(Node, fun() -> register(Name, spawn(fun() -> loop(Name,Mod,Mod:init()) end)) end).

swap_code(Name, Mod) -> rpc(Name, {swap_code, Mod}). 
load_code(Name, Mod) -> 
    {Mod, Binary, _Filename} = code:get_object_code(Mod),
    io:format('sending request~n',[]),
    rpc(Name, {load_code, {Mod, Binary, "fubar.erl"}}).
ping(Name) ->
    rpc(Name, {ping}).

rpc(Name, Request) -> 
    Name ! {self(), Request}, 
    io:format('sent ~w, waiting on response ~n', [Request]),
    receive 
        {Name, Response} -> Response;
        Any -> 
            io:format( "Got unknown response ~p~n", [Any]),
            Any
    end. 

loop(Name, Mod, OldState) -> 
    io:format('into loop~n',[]),
    receive 
        {From, ping} ->
            From ! {pong},
            loop(Name, Mod, OldState);

        {From, {swap_code, NewCallBackMod}} -> 
            From ! {Name, ack}, 
            loop(Name, NewCallBackMod, OldState); 

        {From, {load_code, {Module, Binary, Filename}}} ->
            From ! {got_it},
            io:format("got load code request~n",[]),
            Response = code:load_binary(Module, Binary, Filename),
            io:format("got load code response ~w~n",[Response]),
            From ! Response,
            loop(Name, Mod, OldState);

        {From, Request} -> 
            {Response, NewState} = Mod:handle(Request, OldState), 
            From ! {Name, Response}, 
            loop(Name, Mod, NewState);

        Any ->
            io:format( "Recieved unmatched request ~p ~n",[Any]),
            loop(Name, Mod, OldState)
    end. 
