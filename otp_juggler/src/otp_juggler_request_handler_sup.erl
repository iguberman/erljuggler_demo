%%%-------------------------------------------------------------------
%% @doc otp_juggler top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(otp_juggler_request_handler_sup).

-behaviour(supervisor).

-export([
  start_link/0,
  start_request_handler/1]).

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

  %% JUST an example, won't really be used by the demo (except in comments)
  {ok, RemoteDBHost} = application:get_env(otp_juggler_app, remote_db_host, "localhost"),

    RequestHandlerSpec =
    #{id => otp_juggler_request_handler,
      %%% NOTE this first argument "RemoteDBHost" will be common for all the children created on the fly
      %%% We won't really be talking to any remote DB, but it's a good example of a common argument that's part of the
      %%% "simple_one_for_one" child "template"
      start => {otp_juggler_request_handler, start_link, [RemoteDBHost]},
      restart => temporary,
      shutdown => 5000},
    ChildSpecs = [RequestHandlerSpec],
    {ok, {RestartStrategy, ChildSpecs}}.


%%% FOR a simple_one_for_one supervisor it is essential to have a public method for starting children dynamically
start_request_handler(RequestNumber)->
  %%% IMPORTANT NOTE!   based on the "start" field in the child spec above
  %%% start => {otp_juggler_request_handler, start_link, [RemoteDBHost]},
  %%% this line actually calls
  %%% otp_juggler_request_handler:start_link(RemoteDBHost, RequestNumber)
  %%% The "common" arg (here RemoteDBHost) and the request-"specific" arg(s) (here RequestNumber)
  %%%  get concatenated into the arg list of the call. That's simple_one_for_one implementation magic! :)
  supervisor:start_child(?MODULE, [RequestNumber]).




%% internal functions
