%%%-------------------------------------------------------------------
%%% @author iguberman
%%% @copyright (C) 2019, Xaptum, Inc.
%%% @doc
%%%
%%% @end
%%% Created : 11. Dec 2019 1:21 PM
%%%-------------------------------------------------------------------
-module(otp_juggler_sup).
-author("iguberman").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]
  }} |
  ignore |
  {error, Reason :: term()}).
init([]) ->
  %%% ATTEMPT RESTART every 5 seconds no more than 1000 attempts
  RestartStrategy = #{
    strategy => one_for_one,
    intensity => 5,
    period => 1000},


%%  permanent?  -- long living vital processes should ALWAYS be restarted
%%  temporary?  -- never restarted
%%  transient?  -- a process should be restarted only when it fails, if it terminated normally -- no restart!


  %% Make request timeout configurable
  {ok, RequestTimeout} = application:get_env(otp_juggler_app, juggler_request_timeout, 5000),

  AcceptorSpec =
    #{id => otp_juggler_acceptor,
      start => {otp_juggler_acceptor, start_link, [RequestTimeout]},
      restart => permanent,
      type => worker,   %% default so this line can be ommitted
      shutdown => 5000},

  JugglerRequestHandlerSupSpec =
    #{id => otp_juggler_request_handler_sup,
    start => {otp_juggler_request_handler_sup, start_link, []},
    restart => permanent,
      type => supervisor,
    shutdown => 10000},

  ChildSpecs = [AcceptorSpec, JugglerRequestHandlerSupSpec],

  {ok, {RestartStrategy, ChildSpecs}}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
