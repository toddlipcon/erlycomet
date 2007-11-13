-module(bayeux_http).
-author('telarson@gmail.com').

-export([process_bayeux_message/1]).

%% input: json object. output: json object result of message processing.
process_bayeux_message({Type, Msgs}) ->
    case Type of
	array  -> {array, [ process_message(M) || M <- Msgs ]};
	struct -> {array, [ process_message(Msgs) ]}
    end.

process_message(Msg) ->
    case get_bayeux_val("channel", Msg) of
	"/meta/handshake" ->
	    process_handshake(Msg);
	_ -> ok
    end,
    ok.

get_bayeux_val(Key, Pairs) ->
    [ V | _Rest ] = [ V || {K, V} <- Pairs, K =:= Key],
    V.

process_handshake(_Msg) ->
    Id = generate_id(),
    %% take appropriate action with functions in erlycomet_cluter
    {struct,[{"channel","/meta/handshake"},
	     {"version",1.00000},
	     {"minimumVersion",1.00000},
	     {"clientId", Id},
	     {"supportedConnectionTypes",
	      {array,["long-polling","callback-polling"]}}]}.

% Note: need to call crypto:start() for this to work.  Add to app?
generate_id() ->
    <<Num:128>> = crypto:rand_bytes(16),
    [HexStr] = io_lib:fwrite("~.16B",[Num]),
    HexStr.
