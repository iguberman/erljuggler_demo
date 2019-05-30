defmodule RequestDispatcher do
  @moduledoc false

  require RequestManager
  use GenServer

  def start_link() do
    start_link(%{}, [])
  end



  def start_link(state, opts) do
    state = %{total_requests: 0, finished: 0, total_duration: 0, inf: 0, killed: 0}
    GenServer.start_link(__MODULE__, state, [name: __MODULE__])
  end

  def init(opts) do
    IO.write("Starting #{inspect(__MODULE__)} at #{inspect(self)} with OPTS #{inspect(opts)}\n")
    p_info = Process.info(self, [:registered_name])
    IO.write("#{inspect(p_info)}\n")
    {:ok, opts}
  end

  def handle_call(req, _from, state) when is_integer(req) do
    {:reply, :ok, state}
  end

  def handle_cast(req,  %{total_requests: total_requests} = state) when is_integer(req) do
    {:ok, req_manager} = DynamicSupervisor.start_child(JugglerServer.RequestHandlerSupervisor, RequestManager)
    GenServer.cast(req_manager, {req, self})
    {:noreply, %{state | total_requests: total_requests + 1}}
  end



  def handle_info({:request_stats, num_requests},
          %{total_requests: num_requests, finished: finished, killed: killed, total_duration: duration}) do
    case (finished + killed) do
       total when total >= num_requests ->
          IO.write("FINISHED: #{inspect(finished)} at average duration of #{inspect(duration/finished)}\n")
          IO.write("KILLED: #{inspect(killed)}\n")
       less_than_num_requests ->
          :erlang.send_after(500, self(), {:request_stats, num_requests})
    end
  end
  def handle_info({:request_stats, num_requests}, %{total_requests: total_requests} = state)
      when total_requests < num_requests do
      :erlang.send_after(500, self(), {:request_stats, num_requests})
  end

  def handle_info(:dont_wait_for_me, %{inf: infinite_processes} = state) do
     {:noreply,  %{state | inf: infinite_processes + 1}}
  end
  def handle_info({:finished, duration}, %{finished: finished, total_duration: total_duration} =  state) do
     {:noreply,  %{state | finished:  finished + 1}, total_duration: total_duration + duration}
  end
  def handle_info(%{killed: killed} =  state) do
     {:noreply,  %{state | killed:  killed + 1}}
  end

end