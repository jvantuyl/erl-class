-module(cmd_app).
-behavior(application).

-export([start/2,stop/1]).

start(_Type,_Args) ->
  {ok,TopPid} = cmd_sup:start_link(),
  {ok,TopPid,none}.

stop(_State) ->
  ok.
