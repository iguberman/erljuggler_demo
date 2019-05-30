defmodule RequestManager do
  @moduledoc false

  use GenServer

  def start_link(initial_state) do
    GenServer.start_link(__MODULE__, initial_state, [])
  end

  def init(initial_state) do
    {:ok, initial_state}
  end

  def handle_call(_msg, _from, state) do
    {:reply, :ok, state}
  end

  def handle_cast({req, dispatcher_pid}, state) when is_pid(dispatcher_pid) do
    {:ok, req_handler} = DynamicSupervisor.start_child(JugglerServer.RequestHandlerSupervisor, RequestHandler)
    start_time = :os.system_time(:millisecond)
    try do
      resp = GenServer.call(req_handler, {req, dispatcher_pid}, 500)
    rescue e ->
        IO.write("TIMEOUT ERROR? #{inspect(e)}\n")
        Process.exit(req_handler, :timedout)
    end
    end_time = :os.system_time(:millisecond)
    duration = end_time - start_time
    IO.write("...#{inspect(req)} [[#{inspect(duration)}]]...")
    send(dispatcher_pid, {:finished, duration})
    {:stop, :normal, state}
  end
end