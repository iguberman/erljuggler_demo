%%%-------------------------------------------------------------------
%%% @author iguberman
%%% @copyright (C) 2019, Xaptum, Inc.
%%% @doc
%%%
%%% @end
%%% Created : 11. Dec 2019 1:24 PM
%%%-------------------------------------------------------------------
-module(otp_juggler_acceptor).
-author("iguberman").

-behaviour(gen_server).

%% API
-export([
  start_link/0,
  get_stats/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

%% TODO define the necessary process state "variables" here, any process lifecycle info goes here
%% I.e. let's keep track of some basic request processing stats here
-record(state, {num_total_requests = 0, num_failed_requests = 0, num_ok_requests = 0, duration = 0}).

%%%===================================================================
%%% API
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


get_stats()->
  gen_server:call(?SERVER, get_stats).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call(get_stats, _From, #state{ num_total_requests = NumRequests,
  num_failed_requests = NumFailedRequests,
  num_ok_requests = NumOkRequests,
  duration = TotalDuration} = State) ->
  {reply, #{num_requests => NumRequests,
    num_failed_requests => NumFailedRequests,
    num_ok_requests => NumOkRequests,
    duration => round(TotalDuration/NumOkRequests)}, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast({new_request, ReqNum} = Request , #state{ num_total_requests = NumRequests} = State) ->

  io:format("NEW REQUEST ~p....", [ReqNum]),
  % 1. spawn otp_juggler_request_controller through the otp_juggler_request_controller_sup(ervisor) and capture its Pid
  {ok, Pid} = otp_juggler_request_controller_sup:start_request_controller(ReqNum),

  gen_server:cast(Pid, Request),

  {noreply, State#state{num_total_requests = NumRequests + 1}};
%% Async expect response from controllers
handle_cast({response, Request, timeout}, #state{num_failed_requests = NumFailedRequests} = State) ->
  io:format("Request #~p timed out ~n", [Request]),
  {noreply, State#state{num_failed_requests = NumFailedRequests + 1}};
handle_cast({response, Request, _OkResp, ReqDuration}, #state{num_ok_requests = NumOkRequests, duration = TotalDuration} = State) ->
  io:format("Request #~p successful~n", [Request]),
  {noreply, State#state{num_ok_requests = NumOkRequests + 1, duration = TotalDuration + ReqDuration}}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(Reason, _State) ->
  io:format("~p terminated due to ~p~n", [?SERVER, Reason]),
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
