-module(twilio_app).

-behaviour(application).

-export([start/0, start/2, stop/1]).

start() -> application:ensure_all_started(twilio).

start(_StartType, _StartArgs) ->
    twilio_sup:start_link().

stop(_State) -> ok.
