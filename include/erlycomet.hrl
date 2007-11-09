-ifdef(debug). 
-compile(export_all).
-define(D(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])). 
-else. 
-define(D(X), void). 
-endif.

