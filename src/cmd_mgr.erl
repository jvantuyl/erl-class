-module(cmd_mgr).
-behavior(gen_server).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-export([start_link/0,register_command/2,execute_command/2]).

-record(cmd_mgr_state,{commands}).

% API
start_link() -> gen_server:start_link({global,cmd_mgr},?MODULE,default_commands(),[]).

default_commands() -> []. % Add any "default" commands here.

register_command(CommandName,CommandFun) ->
  gen_server:call({global,cmd_mgr},{register,{CommandName,CommandFun}}).

execute_command(CommandName,Args) -> 
  case gen_server:call({global,cmd_mgr},{command,CommandName}) of
    command_not_found -> bad_command;
    {found,Fun} -> apply(Fun,Args)
  end.

% Callbacks
init(Commands) -> {ok,#cmd_mgr_state{commands=Commands}}.

handle_call({register,{_,_} = Command},_From,#cmd_mgr_state{commands = Commands} = State) ->
  { reply, added, State#cmd_mgr_state{commands = [Command | Commands]} };

handle_call({command,CmdName},_From,#cmd_mgr_state{commands = Commands} = State) ->
  case proplists:get_value(CmdName, Commands) of
    undefined -> {reply,command_not_found,State};
    Fun when is_function(Fun) -> {reply,{found,Fun},State}
  end.

handle_cast(not_implemented,State) -> {noreply,State}.
handle_info(not_implemented,State) -> {noreply,State}.
code_change(_OldVsn,State,_Extra) -> {ok,State}.
terminate(_Reason,_State) -> terminated.
