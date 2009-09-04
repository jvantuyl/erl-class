-module(cmd_sup).
-behavior(supervisor).

-export([start_link/0,init/1]).

start_link() ->
  supervisor:start_link(?MODULE,[]).

init(_Args) ->
  Restart = {one_for_all,5,1},
  Children = [
      {mgr,{cmd_mgr,start_link,[]},permanent,brutal_kill,worker,[cmd_mgr]},
      {ctl,{cmd_ctl,start_link,[]},permanent,brutal_kill,worker,[cmd_ctl]}
    ],
  {ok,{Restart,Children}}.
