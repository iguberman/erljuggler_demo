defmodule RequestHandler do
  @moduledoc false

  use GenServer

  @bug_mod 10

  ### GEN SERVER CALLBACKS

  def start_link(initial_state) do
    GenServer.start_link(__MODULE__, initial_state, [])
  end

  def init(initial_state) do
    {:ok, initial_state}
  end

  def handle_call(req, _from, state) do
    :ok = handle_request(req)
    {:stop, :normal, :ok, state}
  end

  def handle_cast(_msg, state) do
    {:noreply, state}
  end


  ### INTERNAL FUNCTIONS

  def handle_request({req, dispatcher_pid}) when is_integer(req) do
    case Integer.mod(req, @bug_mod) do
      0 ->
        IO.write("\n**** #{inspect(req)} [INF]*****\n")
        send(dispatcher_pid, :dont_wait_for_me)
        handle_with_inf_loop_bug()
      _other ->
        resp = count_to_1000_and_do_other_stuff_too(req, 0)
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

end