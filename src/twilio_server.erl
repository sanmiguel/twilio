-module(twilio_server).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
          api_user = ""
         }).

start_link() ->
        gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) -> 
    TwilioAcct = application:get_env(twilio, api_user, ""),
    {ok, #state{api_user=TwilioAcct}}.

handle_call({new_creds}, _From, State) ->
    #state{api_user=TwilioAcct} = State,
    case twilio_req:new_nat_token(TwilioAcct) of
        {error,_}=Err -> {reply, Err, State};
        [_|_]=JSON -> {reply, {ok, JSON}, State}
    end;
handle_call(_Request, _From, State) ->
        {noreply, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
