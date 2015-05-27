-module(twilio_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Base = application:get_env(twilio, uri_domain, "api.twilio.com"),
    APIUser = application:get_env(twilio, api_user, ""),
    APIToken = application:get_env(twilio, api_token, ""),
    APIVersion = application:get_env(twilio, api_version, "2010-04-01"),

    URI = [ "https://", APIUser, ":", APIToken, "@", Base, "/", APIVersion ],
    ok = application:set_env(twilio, uri_root, lists:flatten(URI)),
    twilio_sup:start_link().

stop(_State) -> ok.
