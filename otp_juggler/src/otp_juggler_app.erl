%%%-------------------------------------------------------------------
%% @doc otp_juggler public API
%% @end
%%%-------------------------------------------------------------------

-module(otp_juggler_app).

-behaviour(application).

%% application behaviour callbacks
-export([start/2, stop/1]).

-export([test_requests/1]).

-export([
    send_request/1,
    get_stats/0]).

start(_StartType, _StartArgs) ->
    otp_juggler_sup:start_link().

stop(_State) ->
    ok.

send_request(Request) ->
    gen_server:cast(otp_juggler_acceptor, {new_request, Request}).

get_stats()->
    otp_juggler_acceptor:get_stats().

test_requests(NumRequests)->
    [otp_juggler_app:send_request(X) || X <- lists:seq(1, NumRequests)].


