%%%-------------------------------------------------------------------
%% @doc otp_juggler public API
%% @end
%%%-------------------------------------------------------------------

-module(otp_juggler_app).

-behaviour(application).

%% application behaviour callbacks
-export([start/2, stop/1]).

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
    io:format("WHEREIS otp_juggler_acceptor: ~p~n", [whereis(otp_juggler_acceptor)]),
    otp_juggler_acceptor:get_stats().


