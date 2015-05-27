-module(twilio_req).

-compile([export_all]).

get_account(AccountID) ->
    get_account(uri(), AccountID).

get_account(URIRoot, AccountID) ->
    ResourceURI = string:join([URIRoot, "Accounts", AccountID++".json"], "/"),
    Req = {ResourceURI, []},
    case httpc:request(get, Req, [], opts()) of
        {ok, {{_, 200, _}, Hdrs, Body}} ->
            %% TODO Can we solicit a binary body from httpc?
            decode(lists:keyfind("content-type", 1, Hdrs), list_to_binary(Body));
        {ok, {{_, N, _}, _, Body}} ->
            {error, {N, Body}};
        {error,_}=Err ->
            Err
    end.

new_nat_token(AccountID) ->
    new_nat_token(uri(), AccountID, 86400).
new_nat_token(AccountID, TTL) ->
    new_nat_token(uri(), AccountID, TTL).

new_nat_token(URIRoot, AccountID, TTL) ->
    ResourceURI = string:join([URIRoot, "Accounts", AccountID, "Tokens.json"], "/"),
    ReqHdrs = [],
    ContentType = "application/x-www-form-urlencoded",
    TTLBin = integer_to_binary(TTL),
    ReqBody = <<"Ttl=", TTLBin/binary>>,
    Req = {ResourceURI, ReqHdrs, ContentType, ReqBody},
    case httpc:request(post, Req, [], opts()) of
        {ok, {{_, 201, _}, Hdrs, Body}} ->
            decode(lists:keyfind("content-type", 1, Hdrs), Body);
        {ok, {{_, N, _}, _, Body}} ->
            {error, {N, Body}};
        {error,_}=Err ->
            Err
    end.
%    } = httpc:request(post, {binary_to_list(URL),
%                             Headers ++ [{"Authorization", "Basic Z3Jhc3M6aG9wcGVy"}],
%                             ContentType, jsx:encode(Body)},
%                      [], []),

decode({"content-type", "application/json"}, Body) ->
    jsx:decode(Body).

uri() ->
    Base = application:get_env(twilio, uri_domain, "api.twilio.com"),
    APIUser = application:get_env(twilio, api_user, ""),
    APIToken = application:get_env(twilio, api_token, ""),
    APIVersion = application:get_env(twilio, api_version, "2010-04-01"),

    URI = [ "https://", APIUser, ":", APIToken, "@", Base, "/", APIVersion ],
    lists:flatten(URI).

opts() -> [{body_format, binary}].
