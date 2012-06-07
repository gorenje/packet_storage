-module(fact_server).
-export([start/1]).

start(Num) ->
    Pid = spawn(fun() -> loop() end),
    Pid1 = spawn(fun() -> loop() end),
    Pid2 = spawn(fun() -> loop() end),
    Pid4 = spawn(fun() -> loop() end),
    Pid3 = spawn(fun() -> loop() end),
    Pid ! {dec,Num,[Pid1,Pid2,Pid3,Pid4]},
    Pid ! {fac,Num,1,[Pid1,Pid2,Pid3,Pid4]}.

loop() ->
    receive
        {fac,0,Val,_} ->
            io:format("Fac: ~w~w~n",[Val,self()]),
            loop();
        {fac,Num,Val,[First|Rest]} ->
            io:format("Part Fac: ~w~w~n",[Val,self()]),
            First ! {fac,Num-1,Num*Val,Rest++[First]},
            loop();
        {dec,0,_} ->
            loop();
        {dec,Num,[First,Second|Rest]} ->
            io:format("Dec: ~w~w~n",[Num,self()]),
            Second ! {fac, Num-1, 1, Rest++[First,Second]},
            First ! {dec, Num-1, Rest++[Second,First]},
            loop()
    end.
