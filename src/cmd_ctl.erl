-module(cmd_ctl).
-behavior(gen_server).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-export([start_link/0]).

% API
start_link() -> 
  gen_server:start_link({global,cmd_ctl},?MODULE,[],[]).

cmd_stop() ->
  init:stop().

% Callbacks
init(_Args) -> 
  cmd_mgr:register_command(exit,fun cmd_stop/0),
  {ok,none}.
handle_call(not_implemented,_From,State) -> {noreply,State}.
handle_cast(not_implemented,State) -> {noreply,State}.
handle_info(not_implemented,State) -> {noreply,State}.
code_change(_OldVsn,State,_Extra) -> {ok,State}.
terminate(_Reason,_State) -> terminated.
