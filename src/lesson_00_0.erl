-module(lesson_00_0).

-author('kagato@souja.net').

-export([count/1]).

count(N) when is_integer(N), N > 0 ->
  do_count(N,N,[]).

do_count(0,N,Accum) when is_integer(N), is_list(Accum) -> Accum;
do_count(X,N,Accum) 
  when is_integer(X), is_integer(N), is_list(Accum), (X =< N), (X > 0) ->
  do_count(X - 1,N,[X | Accum]).
