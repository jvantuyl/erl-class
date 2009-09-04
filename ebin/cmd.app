{application,
  cmd,
  [
    {description, "Command and Control App"},
    {vsn, "0.1"},
    {modules,[cmd_app,cmd_sup,cmd_mgr,cmd_ctl]},
    {registered, [cmd_mgr,cmd_ctl]},
    {applications, [kernel, stdlib, sasl]},
    {mod, {cmd_app,[]}},
    {env, []}
 ]
}.
