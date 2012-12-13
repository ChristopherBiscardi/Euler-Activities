-module(ans).
-export([divisible/1]).

divisible(X) -> 
  if
    (X rem 19 == 0) and 
    (X rem 18 == 0) and 
    (X rem 17 == 0) and 
    (X rem 16 == 0) and 
    (X rem 15 == 0) and 
    (X rem 14 == 0) and 
    (X rem 13 == 0) and 
    (X rem 12 == 0) and 
    (X rem 11 == 0)->
      X;
    true -> divisible(X+20)
  end.

% All numbers below 11 are divisors of a number above 11
% number is divisible by 20 because we are incrementing by 20
% ans:divisible(20).
