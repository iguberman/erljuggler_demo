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

  def generate_requests(num_requests) do
    for x <- :lists.seq(1, num_requests) do
      dispatch(x, self)
    end
  end

#  in place of accept loop
  def accept(num_requests) do
    init_inf_count()
    self = self()
    for x <- :lists.seq(1, num_requests) do
      dispatch(x, self)
    end
    collect_responses(0, 0, 0, 0, num_requests, 0)
  end

  def collect_responses(n, finished, _killed, inf, n, total_duration) do
    IO.write("\n #{inspect(finished)} finished at ave of #{inspect(total_duration/finished)} millis\n")
    case inf do
       0 -> :ok
       positive  -> IO.write("#{inspect(inf)} running forever\n")
    end
  end

  def collect_responses(n, finished, killed, inf, total, total_duration) do
    receive do
      {_handlerPid, :ok, duration} -> collect_responses(n+1, finished+1, killed, inf, total, total_duration + duration)
      {error, :dont_wait_for_me} ->
        curr_inf = Process.get(:inf)
        Process.put(:inf, curr_inf + 1)
        collect_responses(n + 1, finished, killed, inf+1, total, total_duration)
    end
  end

  def dispatch(req, acceptorPid) do
    spawn(Juggler, :kick_off_request_handler, [req, acceptorPid])
  end

  ### a kind of a supervisor
  def kick_off_request_handler(req, acceptor_pid) do
    requestHandlerPid = spawn(Juggler, :handle_request, [req, self()])
    start_time = :os.system_time(:millisecond)
    receive do
      {requestHandlerPid, resp} ->
        end_time = :os.system_time(:millisecond)
        duration = end_time - start_time
        IO.write("...#{inspect(req)} [[#{inspect(duration)}]]...")
        send(acceptor_pid ,{requestHandlerPid, resp, duration})
      other -> send(acceptor_pid, {:error, other})
    end
  end


  def handle_request(req, parentPid) when is_integer(req) do
    case Integer.mod(req, @bug_mod) do
      0 ->
          IO.write("\n**** #{inspect(req)} [INF]*****\n")
          send(parentPid, :dont_wait_for_me)
          handle_with_inf_loop_bug()
      _other ->
          resp = count_to_1000_and_do_other_stuff_too(req, 0)
          handlerPid = self()
          send(parentPid, {handlerPid, resp})
    end
  end

  def count_to_1000_and_do_other_stuff_too(_req, 1000) do
    :ok
  end
  def count_to_1000_and_do_other_stuff_too(req, c) do
    case (rem(req, 2)) do
      0 ->  :binary.copy(<<req::integer>>,300)
      1 ->  :binary.copy(<<(req + 1)::integer>>,200)
    end
    count_to_1000_and_do_other_stuff_too(req, c+1)
  end

  def handle_with_inf_loop_bug() do
      infinite_loop(0)
  end

  def infinite_loop(c) do
    _a = :binary.copy(<<1>>,200)
    _b = :math.sqrt(1235)
    infinite_loop(c+1)
  end


  ## STATS UTILITIES
  ## utility for keeping current count of infinte loops around
  def init_inf_count() do
    case Process.get(:inf) do
      some_int when is_integer(some_int) -> :ok
      _other -> Process.put(:inf, 0)
    end
  end

  def process_stats() do
    Process.put(:running, 0)
    processes = :erlang.processes()
    num_processes = length(processes)
    IO.write("NUM_PROCESSES IN THE SYSTEM: #{inspect(num_processes)}\n")
    for p <- :erlang.processes() do 
	    print_status(p, Process.info(p, :status))
    end
  end

  def print_status(pid, {:status, :running}) do
    num_running = Process.get(:running) + 1
    IO.write("\n\n#{inspect(pid)} * * * RUNNING PROCESS ##{inspect(num_running)}!!! \n")
    Process.put(:running, num_running)
  end

  def print_status(pid, {:status, other_status}) do
    num_procs =
      case Process.get(other_status) do
        nil -> 1
        some_int when is_integer(some_int) -> some_int + 1
      end
    Process.put(other_status, num_procs)
    IO.write("#{inspect(pid)} [#{inspect(other_status)}]...\n")
  end
end