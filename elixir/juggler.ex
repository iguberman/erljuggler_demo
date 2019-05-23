defmodule Juggler do
  @moduledoc false

 @bug_mod 10
 @dont_wait_for_me :dont_wait_for_me

  # NOT USED For DEMO
#  accept_loop() ->
#    Self = self(),
#    receive
#      Req -> dispatch(Req, Self)
#    end,
#    accept_loop().

#  in place of accept loop
  def accept(num_requests) do
    init_inf_count()
    self = self()
    for x <- :lists.seq(1, num_requests) do  dispatch(x, self) end
    collect_responses(0, 0, 0, 0, NumRequests)
  end

  def collect_responses(n, finished, _killed, inf, n) do
    IO.write("\n#{inspect(finished)} finished, #{inspect(Inf)} running forever\n")
  end

  def collect_responses(n, finished, killed, inf, total) do
    receive do
      {_handlerPid, :ok, _duration} -> collect_responses(n+1, finished+1, killed, inf, total)
      {error, :dont_wait_for_me} ->
        curr_inf = Process.get(inf)
        Process.put(inf, CurrInf + 1)
        collect_responses(N + 1, Finished, Killed, Inf+1, Total)
    do
  end

  def dispatch(req, acceptorPid) do
    spawn(Juggler, :kick_off_request_handler, [req, acceptorPid])
  end

  %%% a kind of a supervisor
  def kick_off_request_handler(req, acceptorPid) do
    self = self()
    requestHandlerPid = spawn(Juggler, :handle_request, [req, self])
    start_time = :os.system_time(:millisecond)
    receive do
      {requestHandlerPid, resp} ->
        end_time = :os.system_time(:millisecond)
        duration = end_time - start_time
        IO.write("...#{inspect(Req)} [#{inspect(duration)}]...")
        acceptorPid ! {requestHandlerPid, resp, duration}
      other -> acceptorPid ! {:error, other}
    end.


  def handle_request(Req, ParentPid) when is_integer(Req) do
    case Integer.mod(req, @bug_mod) do
      0 ->
          IO.write("~n**** #{inspect(req)} [INF]*****~n")
          parentPid ! :dont_wait_for_me
          handle_with_inf_loop_bug()
      _other ->
          resp = count_to_1000_and_do_other_stuff_too(req, 0)
          handlerPid = self()
          parentPid ! {handlerPid, resp}
    end
  end

  def count_to_1000_and_do_other_stuff_too(_Req, 1000), :do :ok
  def count_to_1000_and_do_other_stuff_too(Req, C) do
    case (req rem 2) do
      0 ->  :binary.copy(<<req/integer>>,300)
      1 ->  :binary.copy(<<(req + 1)/integer>>,200)
    end
    count_to_1000_and_do_other_stuff_too(Req, C+1)
  end

  def handle_with_inf_loop_bug() do
      infinite_loop(0)
  end

  def infinite_loop(C) do
    _a = :binary.copy(<<1>>,200)
    _b = Math.sqrt(1235)
    infinite_loop(C+1)
  do



  ## STATS UTILITIES

  %% utility for keeping current count of infinte loops around
  def init_inf_count() do
    case get(inf) do
      some_int when is_integer(some_int) -> :ok
      _Other -> put(inf, 0)
  end

  def process_stats() do
    Process.put(running, 0)
    Processes = :eprocesses()
    io:format("NUM_PROCESSES IN THE SYSTEM: ~b~n", [length(Processes)]),
    [print_status(P, {erlang:process_info(P, [status])})
      || P <- :erlang.processes()]  end {:status, :running}]}) do
    num_running = Process.get(:running) + 1
    io:format("~n~n~p * * * RUNNING PROCESS #~b!!! ~n", [Pid, NumRunning])
    Process.put(:running, num_running)
  end


  def print_status(Pid, {[{:status, other_status}]}) do
    num_procs =
      case Process.get(other_status) do
        nil -> 1
        some_int when is_integer(some_int) -> some_int + 1
      end
    Process.put(other_status, num_procs)
    IO.write("#{inspect(pid)} [#{inspect(other_status)}]...\n")
  end
end