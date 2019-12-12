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
-export([start_link/0]).

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
-record(state, {num_total_requests = 0, num_failed_requests = 0, num_ok_requests = 0, request_timeout}).

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
init([RequestTimeout]) ->
  {ok, #state{request_timeout = RequestTimeout}}.

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
  num_ok_requests = NumOkRequests} = State) ->
  {reply, #{num_requests => NumRequests, num_failed_requests => NumFailedRequests, num_ok_requests => NumOkRequests}, State}.


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
handle_cast(Request, #state{ num_total_requests = NumRequests,
  num_failed_requests = NumFailedRequests,
  num_ok_requests = NumOkRequests,
  request_timeout = RequestTimeout} = State) ->
  %%% TODO implement!
  % 1. spawn otp_juggler_request_handler through the otp_juggler_request_handler_sup(ervisor) and capture its Pid
  Pid = otp_juggler_request_handler_sup:start_request_handler(NumRequests + 1),
  Reply =
    try gen_server:call(Pid, Request, RequestTimeout) catch
      CrashError:Reason -> {error, {CrashError, Reason}} end,

  NewState =
    case Reply of
      {error, _Error} ->
        State#state{num_failed_requests = NumFailedRequests + 1, num_total_requests = NumRequests + 1};
      _OkReply ->
        State#state{num_ok_requests = NumOkRequests + 1, num_total_requests = NumRequests + 1}
    end,
  {noreply, NewState}.


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
terminate(_Reason, _State) ->
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
