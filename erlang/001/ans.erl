-module(ans).
-export([calc/2]).

% if N == 1000, stop and return the sum value X
calc(N, X) when N >= 1000 -> X;
% if N is divisible by 5 or 3, add it to X and call calc on the next number
calc(N, X) when (N rem 5 == 0) or (N rem 3 == 0) -> calc(N + 1, X + N);
% else just call the next number
calc(N, X) -> calc(N + 1, X).

% Call it in a repl: 
% ans:calc(0,0)
% In this example
% N is a number that increments from 0 to 1000 by 1 for each recursive call
% X is the sum current sum of all Ns that are divisivle by 5 or 3
