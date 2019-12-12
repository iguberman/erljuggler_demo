%%%-------------------------------------------------------------------
%%% @author iguberman
%%% @copyright (C) 2019, Xaptum, Inc.
%%% @doc
%%%
%%% @end
%%% Created : 11. Dec 2019 4:56 PM
%%%-------------------------------------------------------------------
-module(otp_juggler_SUITE).
-author("iguberman").

-include_lib("common_test/include/ct.hrl").

%% API
-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1,
  test_10000_normal_requests/1,
  test_infinite_loop_requests/1]).

all() -> [test_10000_normal_requests, test_infinite_loop_requests].

init_per_suite(Config)->
  application:ensure_all_started(otp_juggler_app),
  Config.

end_per_suite(_Config) ->
  ok.

test_10000_normal_requests(Config)->
  [otp_juggler_app:send_request({normal, X}) || X <- lists:seq(1, 10000)],
  timer:sleep(60000),
  #{num_requests := 10000, num_failed_requests := 0, num_ok_requests := 10000} = otp_juggler_app:get_stats().


test_infinite_loop_requests(Config) ->
  [otp_juggler_app:send_request({infinite, X}) || X <- lists:seq(1, 10000)],
  timer:sleep(20000),
  #{num_requests := 10000, num_failed_requests := 10000, num_ok_requests := 0} = otp_juggler_app:get_stats().


test_10000_mixed_requests(Config) ->
  [otp_juggler_app:send_request({infinite, X}) || X <- lists:seq(1, 1000)],
  [otp_juggler_app:send_request({infinite, X}) || X <- lists:seq(1, 9000)],
  timer:sleep(120000),
  #{num_requests := 10000, num_failed_requests := 1000, num_ok_requests := 9000} = otp_juggler_app:get_stats().

