-module(ans).
-export([fib/3]).

% If X is less than our max value of 4million AND X is even, add it to Z and move to next fib number in sequence.
fib(X,Y,Z) when (X < 4000000) and (X rem 2 =:= 0) -> fib(X+Y,X,Z+X);
% If X is less than our max value of 4million AND X is not even, move to next number in fib.
fib(X,Y,Z) when (X < 4000000) and (X rem 2 =/= 0) -> fib(X+Y,X,Z);
% When we pass our max value of 4million, return Z
fib(_,_,Z) -> Z.


% ans:fib(1,1,0).
