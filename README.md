# twilio
An erlang library for a subset of the [twilio API](https://www.twilio.com/docs/api/rest)

This is not intended to be a client for the entire twilio API. If you want one of those, try [the official site](https://www.twilio.com/docs/libraries).

This is intended to provide the bare minimum functionality for my own needs.

# usage

You'll need to [create your own twilio account](https://www.twilio.com/try-twilio), then fetch your Account SID and API Token [from here](https://www.twilio.com/user/account/voice-sms-mms/getting-started).

Once you have those you'll need to place them in the `twilio` application env, e.g. in a `sys.config` file:

``` erlang
[
  {twilio, [
    {api_user, "{{ your account SID }}"},
    {api_token, "{{ your API token }}"}
  ]}].
```

Build the project using `rebar`:

`rebar get-deps compile`

Start erlang using your config, e.g.:

``` sh
ERL_LIBS=deps erl -pa ebin -config sys -run twilio_app
```

Now you can get all the metadata for your account or create a new token for use with the twilio stun/turn service.

``` erlang
1> {ok, AccSID} = application:get_env(twilio, api_user).
{ok,"AC00000000000000000000000000000000"}
2> twilio_req:get_account(AccSID).
[{<<"sid">>,<<"AC00000000000000000000000000000000">>},
 {<<"owner_account_sid">>,
  <<"AC00000000000000000000000000000000">>},
 {<<"friendly_name">>,
  <<"michael.coles@gmail.com's Account">>},
 {<<"status">>,<<"active">>}|...]}]
3> twilio_req:new_nat_token(AccSID, TTL=60).
[{<<"username">>,
  <<"2e515994480be0098b9ab3772e2b8e9bf2ef02e4a3d132526c1cc9815f7186cb">>},
 {<<"password">>,
  <<"lvmzIrXsROeSGi+P++uJlNyfldTZJ0+F2/71/hFLe5c=">>},
 {<<"account_sid">>,<<"AC00000000000000000000000000000000">>},
 {<<"ttl">>,<<"60">>},
 {<<"ice_servers">>,
  [[{<<"url">>,
     <<"stun:global.stun.twilio.com:3478?transport=udp">>}],
   [{<<"url">>,
     <<"turn:global.turn.twilio.com:3478?transport=udp">>},
    {<<"username">>,
     <<"2e515994480be0098b9ab3772e2b8e9bf2ef02e4a3d132526c1cc9815f7186cb">>},
    {<<"credential">>,
     <<"lvmzIrXsROeSGi+P++uJlNyfldTZJ0+F2/71/hFLe5c=">>}]]},
 {<<"date_created">>,<<"Wed, 27 May 2015 18:59:29 +0000">>},
 {<<"date_updated">>,<<"Wed, 27 May 2015 18:59:29 +0000">>}]
```
