-module(twilio).

%% Interface module
-export([start/0]).
-export([stop/0]).
-export([new_nat_token/0]).

start() -> application:ensure_all_started(twilio).
stop() -> application:stop(twilio).

new_nat_token() ->
    gen_server:call(twilio_server, {new_creds}).
