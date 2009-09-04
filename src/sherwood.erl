-module(sherwood).
-export([start/0,robin_hood/1,friar_tuck/1,sheriff/1]).

start() ->
  spawn(sherwood,robin_hood,[bootstrap]),
  eat_drink_and_be_merry.

robin_hood(bootstrap) ->
  io:format("Behold, the merry men of Sherwood forest!~n"),
  process_flag(trap_exit,true),
  Tuck = spawn_opt(sherwood,friar_tuck,[{start_up,self()}],[link]),
  sherwood:robin_hood({start_up,Tuck});
robin_hood({start_up,Tuck}) ->
  io:format("Robin Hood: Aha! Robin Hood is here!~n"),
  register(robin_hood,self()),
  process_flag(trap_exit,true),
  sherwood:robin_hood(Tuck);
robin_hood(Tuck) when is_pid(Tuck) ->
  receive
    {'EXIT',Tuck,_} ->
      io:format("Robin Hood: Kind friar, you shall not be imprisoned for long!~n"),
      NewTuck = spawn_opt(sherwood,friar_tuck,[{start_up,self()}],[link]),
      sherwood:robin_hood(NewTuck)
  after 15000 ->
    io:format("Robin Hood: We steal from the rich and give to the poor!~n"),
    sherwood:robin_hood(Tuck)
  end.

friar_tuck({start_up,Robin}) ->
  io:format("Friar Tuck: Robin! Your trusty companion Friar Tuck has arrived!~n"),
  register(friar_tuck,self()),
  process_flag(trap_exit,true),
  sherwood:friar_tuck(Robin);
friar_tuck(Robin) when is_pid(Robin) ->
  receive
    {'EXIT',Robin,_} ->
      io:format("Friar Tuck: Fear not, friend Robin!  I shall rout the Sheriff of Nottingham's men!~n"),
      NewRobin = spawn_opt(sherwood,robin_hood,[{start_up,self()}],[link]),
      sherwood:friar_tuck(NewRobin)
  after 20000 ->
    io:format("Friar Tuck: Ale!  Ale for the merry men!~n"),
    sherwood:friar_tuck(Robin)
  end.

sheriff(Who) ->
  case whereis(Who) of
    Pid when is_pid(Pid) ->
      io:format("Sheriff of Nottingham: I have you, ~p!~n",[Who]),
      exit(Pid,kill); % Avra Cadavra!
    undefined ->
      io:format("Sheriff of Nottingham: Can't find ~p.~n",[Who])
  end,
  muhahahaha.
