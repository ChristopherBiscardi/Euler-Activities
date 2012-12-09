-module(ans).
-export([largestPrimeFactor/1,
    smallestDivisor/2,
    factors/2]).

% Because we start with the smallest divisors, the last value is the largest.
largestPrimeFactor(X) -> lists:last(factors(X,[])).

factors(X, N) when N == [] -> K = smallestDivisor(X,2),
  J = hd(K) div lists:last(K), 
  if
    J == 1 ->
      N ++ [lists:last(K)];
    true ->
      factors(J,N ++ [lists:last(K)])
  end;
factors(X, N) -> K = smallestDivisor(X,2),
  J = hd(K) div lists:last(K), 
  if
    J == 1 ->
      N ++ [lists:last(K)];
    true ->
      factors(J,N ++ [lists:last(K)])
  end.

% Take a number X and find Divisor N
% If N divides X evenly, return a tuple containing X and the clean divisor N
smallestDivisor(X,N) when X rem N == 0 -> [X,N];
% else bump up the divisor and try again
smallestDivisor(X,N) -> smallestDivisor(X,N+1).

% ans:largestPrimeFactor(600851475143).
