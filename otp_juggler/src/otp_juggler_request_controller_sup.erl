%%%-------------------------------------------------------------------
%% @doc otp_juggler top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(otp_juggler_request_controller_sup).

-behaviour(supervisor).

-export([
  start_link/0,
  start_request_controller/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->

%%    one_for_one?
%%    one_for_all?
%%    rest_for_one?
%%    simple_one_for_one?


  %%% ATTEMPT RESTART every 5 seconds no more than 1000 attempts
    RestartStrategy = #{
        strategy => simple_one_for_one,
        intensity => 5,
        period => 1000},


%%  permanent?  -- long living vital processes should ALWAYS be restarted
%%  temporary?  -- never restarted
%%  transient?  -- a process should be restarted only when it fails, if it terminated normally -- no restart!


  %% Make request timeout configurable
  RequestTimeout = application:get_env(otp_juggler_app, request_timeout, 1000),

    RequestHandlerSpec =
    #{id => otp_juggler_request_controller,
      %%% NOTE this first argument "RequestTimeout" will be common for all the children created on the fly
      %%% Example of a common argument that's part of the
      %%% "simple_one_for_one" child "template"
      start => {otp_juggler_request_controller, start_link, [RequestTimeout]},
      restart => temporary,
      shutdown => 1000},
    ChildSpecs = [RequestHandlerSpec],
    {ok, {RestartStrategy, ChildSpecs}}.


%%% FOR a simple_one_for_one supervisor it is essential to have a public method for starting children dynamically
start_request_controller(RequestNumber)->
  %%% IMPORTANT NOTE!   based on the "start" field in the child spec above
  %%% start => {otp_juggler_request_handler, start_link, [RequestTimeout]},
  %%% this line actually calls
  %%% otp_juggler_request_handler:start_link(RequestTimeout, RequestNumber)
  %%% The "common" arg (here RequestTimeout) and the request-"specific" arg(s) (here RequestNumber)
  %%%  get concatenated into the arg list of the call. That's simple_one_for_one implementation magic! :)
  supervisor:start_child(?MODULE, [RequestNumber]).




%% internal functions
