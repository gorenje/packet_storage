-module(y).
-export([y/1,seq/1,seq2/1,seq3/1]).

%% y combinator that allows anonymous functions to be recursive. one issue is that
%% the arguments can only be airity of 1 (without extending the y combinator)
%% See http://bc.tech.coop/blog/070611.html for details
y(M) ->
    G = fun (F) -> M(fun(A) -> (F(F))(A) end) end,
    G(G).

%% this is sequence function
seq(N) ->
  (y(fun (F) -> fun ({0,L}) -> L; ({X,L}) -> F({X-1,L++[X]}) end end))({N,[]}).

%% also can be written like:
seq2(1) -> [1];
seq2(X) -> [X|seq2(X-1)].

%% slightly stupidly was my first attempt (and still to be seen in seq)
seq3(X) -> seq3(X,[]).
seq3(0,L) -> L;
seq3(X,L) -> seq3(X-1,L++[X]).
%% one nice thing about seq3 is the use of the arity: seq3/1 has nothing to 
%% do with seq3/2 (different function but same name). But only need to export
%% seq3/1 since that is the interface that should be used.


