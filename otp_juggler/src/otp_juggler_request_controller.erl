%%%-------------------------------------------------------------------
%%% @author iguberman
%%% @copyright (C) 2019, Xaptum, Inc.
%%% @doc
%%%
%%% @end
%%% Created : 11. Dec 2019 10:18 AM
%%%-------------------------------------------------------------------
-module(otp_juggler_request_controller).
-author("iguberman").

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).



-record(state, {request_timeout = 1000, handler, request_num}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(Arg1::list(), Arg2::integer()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(RequestTimeout, RequestNum) ->
%%  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
  io:format("START CONRTOLLER #~p WITH timeout ~p~n", [RequestNum, RequestTimeout] ),
  gen_server:start_link(?MODULE, [RequestTimeout, RequestNum], []).

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
init([RequestTimeout, RequestNum]) ->

  {ok, HandlerPid} = otp_juggler_request_handler_sup:start_request_handler(RequestNum),
  io:format("Starting handler for request #~p: ~p~n", [RequestNum, HandlerPid]),
  {ok, #state{request_timeout = RequestTimeout, handler = HandlerPid, request_num = RequestNum}}.

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
handle_call(UnexpectedReq, _From, State)->
  io:format("UnexpectedReq ~p~n", [UnexpectedReq]).

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
handle_cast(Request, #state{request_timeout = RequestTimeout, handler = HandlerPid, request_num = RequestNum} = State) ->
  Start = os:system_time(millisecond),
  Reply = gen_server:call(HandlerPid, Request, RequestTimeout),
  %% Blocking call
%%  Reply =
%%    try gen_server:call(Pid, Request, RequestTimeout) catch
%%      CrashError:Reason -> {error, {CrashError, Reason}} end,
  Duration = os:system_time(millisecond) - Start,
  %% Non-blocking call if we ever get here
  gen_server:cast(otp_juggler_acceptor, {response, Request, Reply, Duration}),
  {stop, normal, State}.

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
terminate({timeout, Where}, #state{handler = HandlerPid, request_num = RequestNum} = State) ->
  io:format("CONTROLLER ~p TERMINATING due to handler timeout in ~p~n", [self(), Where]),
  exit(HandlerPid, timeout),
  gen_server:cast(otp_juggler_acceptor, {response, RequestNum, timeout}),
  ok;
terminate(normal, #state{handler = HandlerPid, request_num = RequestNum} = State) ->
  io:format("CONTROLLER ~p TERMINATING normally~n", [self()]),
  gen_server:stop(HandlerPid, normal, 100),
  ok;
terminate(Other, #state{handler = HandlerPid, request_num = RequestNum} = State) ->
  io:format("CONTROLLER ~p TERMINATING normally ~p~n", [self(), Other]),
  gen_server:stop(HandlerPid, normal, 100),
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

