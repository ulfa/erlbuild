-define(DEBUG(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).
-define(DEBUG(Var, Var1), io:format("DEBUG: ~p:~p - ~p~n ~p~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var, Var1])).
