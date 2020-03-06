%%%-------------------------------------------------------------------
%%% @author iguberman
%%% @doc
%%%
%%% @end
%%% Created : 27. Feb 2019 7:27 PM
%%%-------------------------------------------------------------------
-module(juggler).
-author("iguberman").

-define(BUG_MOD, 10).

%% API
-export([
  accept_loop/0,
  accept/1,
  kick_off_request_handler/2,
  handle_request/2,
  dispatch/2,
  process_stats/0]).

-define(DONT_WAIT_FOR_ME, dont_wait_for_me).

%%% NOT USED For DEMO
accept_loop() ->
  Self = self(),
  receive
    Req -> dispatch(Req, Self)
  end,
  accept_loop().

%% in place of accept loop
accept(NumRequests)->
  init_inf_count(),
  Self = self(),
  [dispatch(X, Self) || X <- lists:seq(1, NumRequests)],
  collect_responses(0, 0, 0, 0, NumRequests, 0).

collect_responses(N, Finished, _Killed, Inf, N, TotalDuration) ->
  io:format("~n=================================================================================~n"),
  io:format("=================================================================================~n~n"),
  io:format("~b finished at ~b ms on average.~n~b still running~n", [Finished, round(TotalDuration / Finished), Inf]);
collect_responses(N, Finished, Killed, Inf, Total, TotalDuration) ->
  receive
    {_HandlerPid, ok, Duration} ->
      collect_responses(N+1, Finished+1, Killed, Inf, Total, TotalDuration + Duration);
    {error, ?DONT_WAIT_FOR_ME} ->
      CurrInf = get(inf),
      put(inf, CurrInf + 1),
      collect_responses(N + 1, Finished, Killed, Inf+1, Total, TotalDuration)
  end.

dispatch(Req, AcceptorPid) ->
  spawn(?MODULE, kick_off_request_handler, [Req, AcceptorPid]).


%%% a kind of a supervisor
kick_off_request_handler(Req, AcceptorPid) ->
  Self = self(),
  RequestHandlerPid = spawn(?MODULE, handle_request, [Req, Self]),
  Start = os:system_time(millisecond),
  receive
    {RequestHandlerPid, Resp} ->
      End = os:system_time(millisecond),
      Duration = End - Start,
      io:format("...REQ# ~p [~bms]...", [Req, Duration]),
      AcceptorPid ! {RequestHandlerPid, Resp, Duration};
    Other -> AcceptorPid ! {error, Other}
  end.


handle_request(Req, ParentPid) when is_integer(Req) ->
  case Req rem ?BUG_MOD of
    0 ->
        io:format("~n**** ~p [INF]*****~n", [Req]),
        ParentPid ! ?DONT_WAIT_FOR_ME,
        handle_with_inf_loop_bug();
    _Other ->
        Resp = count_to_1000_and_do_other_stuff_too(Req, 0),
        HandlerPid = self(),
        ParentPid ! {HandlerPid, Resp}
  end.

count_to_1000_and_do_other_stuff_too(_Req, 1000) -> ok;
count_to_1000_and_do_other_stuff_too(Req, C) ->
  case (Req rem 2) of
    0 ->  binary:copy(<<Req/integer>>,300);
    1 ->  binary:copy(<<(Req + 1)/integer>>,200)
  end,
  count_to_1000_and_do_other_stuff_too(Req, C+1).

handle_with_inf_loop_bug()->
    infinite_loop(0).

infinite_loop(C) ->
  _A = binary:copy(<<1>>,200),
  _B = math:sqrt(1235),
  infinite_loop(C+1).



%%%% STATS UTILITIES

%% utility for keeping current count of infinte loops around
init_inf_count()->
  case get(inf) of
    SomeInt when is_integer(SomeInt) -> ok;
    _Other -> put(inf, 0)
  end.

process_stats()->
  put(running, 0),
  Processes = erlang:processes(),
  io:format("NUM_PROCESSES IN THE SYSTEM: ~b~n", [length(Processes)]),
  [print_status(P, {erlang:process_info(P, [status])})
    || P <- erlang:processes()].

print_status(Pid, {[{status,running}]}) ->
  NumRunning = get(running) + 1,
  io:format("~n~n~p * * * RUNNING PROCESS #~b!!! ~n", [Pid, NumRunning]),
  put(running, NumRunning);
print_status(Pid, {[{status, Other}]}) ->
  NumProcs =
    case get(Other) of
      undefined -> 1;
      Int when is_integer(Int) -> Int + 1
    end,
  put(Other, NumProcs),
  io:format("~p [~p]...", [Pid, Other]).

